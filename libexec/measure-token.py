#!/usr/bin/env nix-shell
#! nix-shell -i python3 --pure -p python3 python3Packages.tiktoken

import argparse
import sys
import tiktoken


def main():
    parser = argparse.ArgumentParser(description="Measure token count using tiktoken")
    parser.add_argument("file", help="File to measure")
    parser.add_argument(
        "--tokenizer",
        default="o200k_base",
        help="Tiktoken encoding to use (default: o200k_base)",
    )
    args = parser.parse_args()

    try:
        with open(args.file, "r", encoding="utf-8") as f:
            text = f.read()
    except FileNotFoundError:
        print(f"Error: {args.file} not found", file=sys.stderr)
        sys.exit(1)

    enc = tiktoken.get_encoding(args.tokenizer)
    tokens = enc.encode(text)

    print(f"File: {args.file}")
    print(f"Tokenizer: {args.tokenizer}")
    print(f"Tokens: {len(tokens)}")


if __name__ == "__main__":
    main()