#!/usr/bin/python

import argparse
import os
import re

PATH = "/home/orz/code/ertorrent_cp/apps/ertorrent/src"

def compile_erlang_paths(path):
    dir_contents = os.listdir(path)
    erlang_files = []

    for entry in dir_contents:
        if ".erl" in entry:
            erlang_files.append(path + '/' + entry)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("path", help="path to operate in", type=string)
    args = parser.parse_args()

if __name__ == "__main__":
    main()
