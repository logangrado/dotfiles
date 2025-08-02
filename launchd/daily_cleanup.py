#!/usr/bin/env python3

from pathlib import Path
import shutil
import datetime

LOG_FILE = Path.home() / ".dotfiles/launchd/daily_cleanup.log"


def _log(message):
    nonce = datetime.datetime.now().strftime("%y-%m-%d %H:%M:%S.%f")

    with open(LOG_FILE, "a") as f:
        f.write(f"[{nonce}] {message}\n")


def _cleanup(src, dst, age):

    now = datetime.datetime.now().timestamp()
    for item in src.glob("*"):

        item_age = (now - item.stat().st_birthtime) / 60 / 60 / 24

        if item_age > age:
            # Determine dest name, adding suffix as required to avoid conflict
            item_dst_base = dst / item.relative_to(src)
            item_dst = item_dst_base
            i = 0
            while item_dst.exists():
                item_dst = Path(f"{item_dst_base}_{i}")
                i += 1

            # Move the item
            shutil.move(item, item_dst)


def _truncate_log_file(path, max_size):
    if not path.exists():
        return

    if path.stat().st_size < max_size:
        return

    with open(path, "rb") as f:
        lines = f.readlines()

    # Rebuild from end to start until we're within the size limit
    retained_lines = []
    total_size = 0
    for line in reversed(lines):
        total_size += len(line)
        if total_size > max_size:
            break
        retained_lines.append(line)

    with open(path, "wb") as f:
        f.writelines(retained_lines[::-1])


def main():
    _log("Running daily cleanup")

    downloads = Path.home() / "Downloads"
    dirs = [
        (downloads / "0_today", 0),
        (downloads / "1_yesterday", 1),
        (downloads / "2_last_week", 7),
        (downloads / "3_last_month", 30),
        (Path.home() / ".Trash", None),
    ]

    # Create dirs
    for d, _ in dirs:
        d.mkdir(exist_ok=True, parents=True)

    # Move all items from Downloads -> Today
    for item in downloads.glob("*"):
        if item not in [d[0] for d in dirs]:
            item_dst = dirs[0][0] / item.relative_to(downloads)
            shutil.move(item, item_dst)

    # Run cleanup
    for i in range(len(dirs) - 1):
        src, age = dirs[i]
        dst, _ = dirs[i + 1]

        _cleanup(src, dst, age)

    # Cleanup log file
    _truncate_log_file(LOG_FILE, max_size=8 * 1024**2)

    _log("Done\n")


if __name__ == "__main__":
    main()
