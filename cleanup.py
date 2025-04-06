# standardizing file names

import os
from pathlib import Path


def rename_files_in_subdirs(parent_dir):
    parent = Path(parent_dir)

    if not parent.is_dir():
        print(f"{parent_dir} is not a valid directory.")
        return

    for subdir in parent.iterdir():
        if subdir.is_dir():
            r_files = list(subdir.glob("*.R"))
            png_files = list(subdir.glob("*.png"))
            gif_files = list(subdir.glob("*.gif"))

            def handle_file_rename(files, new_name, file_type):
                if len(files) == 1:
                    old_path = files[0]
                    new_path = subdir / new_name
                    if old_path.name != new_name:
                        old_path.rename(new_path)
                        print(f"Renamed {old_path} -> {new_path}")

                elif len(files) > 1:
                    print(f"⚠️ Multiple {file_type} files in {subdir}:")
                    for f in files:
                        print(f"  - {f}")

            handle_file_rename(r_files, "code.R", ".R")
            handle_file_rename(png_files, "image.png", ".png")
            handle_file_rename(gif_files, "animation.gif", ".gif")


rename_files_in_subdirs("tidytuesday/2023")
