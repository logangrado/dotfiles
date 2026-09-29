import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";

const BAR_WIDTH = 16;
const SUBCHARACTER_STEPS = 8;
const PARTIAL_BLOCKS = ["", "▏", "▎", "▍", "▌", "▋", "▊", "▉"];
const SEPARATOR = "·";
const GIT_BRANCH_SYMBOL = "";

type Usage = {
	input: number;
	output: number;
	cacheRead: number;
	cacheWrite: number;
	cost: { total: number };
};

type UsageTotals = Usage;

function emptyUsage(): UsageTotals {
	return { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, cost: { total: 0 } };
}

function addUsage(totals: UsageTotals, usage: Usage | undefined): void {
	if (!usage) return;
	totals.input += usage.input;
	totals.output += usage.output;
	totals.cacheRead += usage.cacheRead;
	totals.cacheWrite += usage.cacheWrite;
	totals.cost.total += usage.cost.total;
}

function formatTokens(tokens: number): string {
	if (tokens < 1_000) return `${Math.round(tokens)}`;

	const thousands = Number((tokens / 1_000).toPrecision(3));
	if (thousands < 1_000) return `${thousands}k`;

	return `${Number((tokens / 1_000_000).toPrecision(3))}m`;
}

function usageColor(percent: number): "success" | "warning" | "error" {
	if (percent >= 90) return "error";
	if (percent >= 70) return "warning";
	return "success";
}

function formatCwd(cwd: string): string {
	const home = process.env.HOME ?? process.env.USERPROFILE;
	return home && cwd.startsWith(home) ? `~${cwd.slice(home.length)}` : cwd;
}

function contextBar(ctx: ExtensionContext): string {
	const usage = ctx.getContextUsage();
	if (!usage || usage.tokens === null || usage.percent === null) {
		return `${ctx.ui.theme.fg("dim", "ctx |")}${ctx.ui.theme.fg("accent", "????????????????")}${ctx.ui.theme.fg("dim", "| ")}${ctx.ui.theme.fg("accent", "calculating")}`;
	}

	const percent = Math.max(0, Math.min(100, usage.percent));
	const units = Math.round((percent / 100) * BAR_WIDTH * SUBCHARACTER_STEPS);
	const fullBlocks = Math.floor(units / SUBCHARACTER_STEPS);
	const partialBlock = units % SUBCHARACTER_STEPS;
	const bar = ctx.ui.theme.fg(
		usageColor(percent),
		"█".repeat(fullBlocks) + PARTIAL_BLOCKS[partialBlock],
	);
	const remainingWidth = BAR_WIDTH - fullBlocks - (partialBlock === 0 ? 0 : 1);
	const remaining = ctx.ui.theme.fg("dim", "░".repeat(remainingWidth));

	const label = `${formatTokens(usage.tokens)}/${formatTokens(usage.contextWindow)} ${Math.round(percent)}%`;
	return `${ctx.ui.theme.fg("dim", "ctx |")}${bar}${remaining}${ctx.ui.theme.fg("dim", "| ")}${ctx.ui.theme.fg("accent", label)}`;
}

function collectUsage(ctx: ExtensionContext): { totals: UsageTotals; latestCacheHitRate?: number } {
	const totals = emptyUsage();
	let latestCacheHitRate: number | undefined;

	for (const entry of ctx.sessionManager.getEntries()) {
		if (entry.type === "message" && (entry.message.role === "assistant" || entry.message.role === "toolResult")) {
			const usage = entry.message.usage as Usage | undefined;
			addUsage(totals, usage);
			if (entry.message.role === "assistant" && usage) {
				const promptTokens = usage.input + usage.cacheRead + usage.cacheWrite;
				latestCacheHitRate = promptTokens === 0 ? undefined : (usage.cacheRead / promptTokens) * 100;
			}
		} else if ((entry.type === "branch_summary" || entry.type === "compaction") && entry.usage) {
			addUsage(totals, entry.usage as Usage);
		}
	}

	return { totals, latestCacheHitRate };
}

export default function (pi: ExtensionAPI): void {
	pi.on("session_start", (_event, ctx) => {
		ctx.ui.setFooter((tui, theme, footerData) => {
			const unsubscribe = footerData.onBranchChange(() => tui.requestRender());

			return {
				dispose: unsubscribe,
				invalidate() {},
				render(width: number): string[] {
					const { totals, latestCacheHitRate } = collectUsage(ctx);
					const stat = (label: string, value: string) =>
						`${theme.fg("dim", label)}${theme.fg("accent", value)}`;
					const usageStats: string[] = [];
					if (totals.input) usageStats.push(stat("↑", formatTokens(totals.input)));
					if (totals.output) usageStats.push(stat("↓", formatTokens(totals.output)));

					const cacheStats: string[] = [];
					if (totals.cacheRead) cacheStats.push(stat("R", formatTokens(totals.cacheRead)));
					if (totals.cacheWrite) cacheStats.push(stat("W", formatTokens(totals.cacheWrite)));
					if (latestCacheHitRate !== undefined) cacheStats.push(stat("CH", `${latestCacheHitRate.toFixed(1)}%`));
					const totalPromptTokens = totals.input + totals.cacheRead + totals.cacheWrite;
					if (totalPromptTokens) {
						cacheStats.push(stat("Σ", `${((totals.cacheRead / totalPromptTokens) * 100).toFixed(1)}%`));
					}

					const sections: string[] = [];
					if (usageStats.length) sections.push(usageStats.join(" "));
					if (cacheStats.length) sections.push(cacheStats.join(" "));
					if (totals.cost.total) sections.push(stat("$", totals.cost.total.toFixed(3)));
					sections.push(contextBar(ctx));
					const left = sections.join(` ${theme.fg("dim", SEPARATOR)} `);

					const model = theme.fg("syntaxString", ctx.model?.id ?? "no-model");
					const thinkingLevel = ctx.thinkingLevel ?? "off";
					const thinking = ctx.model?.reasoning
						? ` ${theme.fg("dim", SEPARATOR)} ${theme.getThinkingBorderColor(thinkingLevel)(thinkingLevel === "off" ? "thinking off" : thinkingLevel)}`
						: "";
					const provider =
						footerData.getAvailableProviderCount() > 1 && ctx.model
							? `${theme.fg("muted", ctx.model.provider)} `
							: "";
					const right = `${provider}${model}${thinking}`;
					const gap = " ".repeat(Math.max(2, width - visibleWidth(left) - visibleWidth(right)));
					const statsLine = truncateToWidth(`${left}${gap}${right}`, width);

					let cwd = theme.fg("syntaxKeyword", formatCwd(ctx.cwd));
					const branch = footerData.getGitBranch();
					if (branch) {
						cwd += ` ${theme.fg("dim", SEPARATOR)} ${theme.fg("mdCodeBlock", `${GIT_BRANCH_SYMBOL} ${branch}`)}`;
					}
					const sessionName = pi.getSessionName();
					if (sessionName) cwd += ` ${theme.fg("dim", SEPARATOR)} ${theme.fg("dim", sessionName)}`;

					const task = process.env.HATCHERY_TASK;
					const taskLabel = task ? `⬮ ${task}` : "";
					const shownCwd = taskLabel
						? truncateToWidth(cwd, Math.max(0, width - visibleWidth(taskLabel) - 2), "...")
						: cwd;
					const taskGap = taskLabel
						? " ".repeat(Math.max(2, width - visibleWidth(shownCwd) - visibleWidth(taskLabel)))
						: "";
					const cwdLine = taskLabel ? `${shownCwd}${taskGap}${theme.fg("accent", taskLabel)}` : cwd;

					return [
						truncateToWidth(cwdLine, width, theme.fg("dim", "...")),
						truncateToWidth(statsLine, width, theme.fg("dim", "...")),
					];
				},
			};
		});
	});

	pi.on("session_shutdown", (_event, ctx) => {
		ctx.ui.setFooter(undefined);
	});
}
