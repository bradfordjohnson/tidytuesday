import os
import argparse
import requests

def download_csv(url, dest_dir="data/raw"):
    os.makedirs(dest_dir, exist_ok=True)
    dest_file = os.path.join(dest_dir, "data.csv")  # fixed name

    response = requests.get(url)
    response.raise_for_status()

    with open(dest_file, "wb") as f:
        f.write(response.content)

    print(f"Downloaded to: {dest_file}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Download CSV as data.csv from a URL.")
    parser.add_argument("url", help="The URL of the CSV file.")
    parser.add_argument("--dir", default="data/raw", help="Destination directory (default: data/raw)")
    args = parser.parse_args()

    download_csv(args.url, args.dir)