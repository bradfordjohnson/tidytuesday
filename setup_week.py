import argparse
from pathlib import Path
from datetime import date, timedelta

ROOT_PATH = Path("tidytuesday")


def parse_date(year, month, day):
    d = date(year, month, day)
    if d.weekday() != 1:
        raise ValueError(f"{d.isoformat()} is not a Tuesday")
    return d


def get_upcoming_tuesday():
    today = date.today()
    days_ahead = 1 - today.weekday()
    if days_ahead <= 0:
        days_ahead += 7
    return today + timedelta(days=days_ahead)


def setup_tidytuesday_project(year: int, month: int, day: int):
    d = parse_date(year, month, day)

    folder_path = ROOT_PATH / str(d.year) / d.isoformat()
    folder_path.mkdir(parents=True, exist_ok=True)

    r_file_path = folder_path / f"{d.isoformat()}.R"
    
    folder_path_str = str(folder_path).replace("\\", "/")
    if not r_file_path.exists():
        with r_file_path.open("w") as f:
            f.write(f'library(tidytuesday)\n\n\nggsave("{folder_path_str}/image.png")\n')
        print(f"Created file: {r_file_path}")
    else:
        print(f"File already exists: {r_file_path}")

    return r_file_path


def main():
    parser = argparse.ArgumentParser(description="Initialize a tidytuesday directory.")
    parser.add_argument("year", type=int, nargs="?", help="Year")
    parser.add_argument("month", type=int, nargs="?", help="Month")
    parser.add_argument("day", type=int, nargs="?", help="Day")

    args = parser.parse_args()

    if args.year is None or args.month is None or args.day is None:
        upcoming_tuesday = get_upcoming_tuesday()
        print(
            f"No date provided. Using upcoming Tuesday: {upcoming_tuesday.isoformat()}"
        )
        setup_tidytuesday_project(
            upcoming_tuesday.year, upcoming_tuesday.month, upcoming_tuesday.day
        )
    else:
        setup_tidytuesday_project(args.year, args.month, args.day)


if __name__ == "__main__":
    main()

# python setup_week.py
# python setup_week.py YYYY MM DD
