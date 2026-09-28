import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

/** Adds a familiar /exit alias for Pi's standard graceful shutdown. */
export default function (pi: ExtensionAPI) {
  pi.registerCommand("exit", {
    description: "Exit Pi",
    handler: async (_args, ctx) => {
      ctx.shutdown();
    },
  });
}
