#!/usr/bin/env python3

import subprocess
import json
import glob

def get_generated_egg():
    program = subprocess.run(["cargo", "run", "--quiet"], cwd="./dag_in_context", capture_output=True)
    return len(program.stdout.splitlines())

def get_written_egg():
    file_paths = glob.glob("./**/*.egg", recursive=True)

    counts = {}
    for file_path in file_paths:
        with open(file_path, 'r') as file:
            if file_path not in counts:
                counts[file_path] = 0
            lines = file.readlines()
            counts[file_path] += len(lines)
    return counts

def get_rust_lines():
    rust_lines_output = subprocess.run(["tokei", "--output", "json", "./"], capture_output=True)
    return json.loads(rust_lines_output.stdout)["Rust"]["code"]


def gen_detailed_table(line_counts):
    fmt = """\\begin{tabular}{ |s|p{2cm}| }
\hline
\multicolumn{2}{|c|}{Egglog Line Counts} \\\
\hline
File & \# Lines  \\\\
\hline"""

    for key in sorted(line_counts.keys()):
        fmt += f"{key} & f{line_counts[key]} \\\\"
        fmt += "\\hline"

    fmt += """
\hline
\end{tabular}"""
    return fmt

def gen_linecount_table():
    rust_lines = str(get_rust_lines())
    generated_egg = str(get_generated_egg())

    counts = get_written_egg()
    written_egg = 0

    for _, v in counts.items():
        written_egg += v


    overview = """\\begin{tabular}{ |s|p{2cm}| }
\hline
\multicolumn{2}{|c|}{Line Counts} \\\
\hline
Language & \# Lines  \\\\
\hline
Rust & %s \\\\
Written Egg & %s \\\\
Generated EGG & %s \\\\
\hline
\end{tabular}""" % (rust_lines, written_egg, generated_egg)

    detailed = gen_detailed_table(counts)

    return (overview, detailed)

