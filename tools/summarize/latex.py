"""
    Simple wrappers for outputting LaTeX
"""

import util

# TeX preamble / header
PREAMBLE = "\n".join(["\\documentclass{article}"
                     ,"\\usepackage{graphicx,enumitem}"
                     ,"\\newcommand{\\mono}[1]{\\texttt{#1}}"
                     ,"\\begin{document}"
                     ,"\\setlist[enumerate,1]{start=0}"
                     ])

def difference(n1, n2):
    """ (-> Nat Nat (List Nat String))
       Rounded quotient (n1 / n2).
       Shows how many times better or worse `n1` is compared to the
       expected `n2`.
       Additionally, returns a descriptive string.
    """
    val = round(n1 / n2, 2)
    if val >= 1:
        descr = "slower"
    else:
        descr = "faster"
    return val, descr

def list(items, numbers=False):
    tag = "enumerate" if numbers else "itemize"
    return "\\begin{%s}\n\\item %s\\end{%s}" % (tag, "\n\item ".join(items), tag)

def end():
    return "\\end{document}"

def figure(fname):
    """
        Splice the filename `fname` into an imported figure
        for the final .tex
    """
    return "\\includegraphics[width=\\textwidth]{%s}" % util.strip_directory(fname)

def section(title):
    return "\n\n\\section{%s}" % title

def subsection(title):
    return "\n\\subsection{%s}" % title

def table(title, rows):
    return "\n".join(["\n\\hspace{-4cm}\\begin{tabular}{%s}\\\\\\hline" % " | ".join(("c" for _ in range(len(title))))
                     ," & ".join((str(x) for x in title)) + "\\\\"
                     ,"\\\\\n".join((" & ".join((str(x) for x in row)) for row in rows))
                     ,"\\end{tabular}"
                     ])