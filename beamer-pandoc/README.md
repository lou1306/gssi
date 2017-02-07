# A Pandoc template for beamer slides

I just took the GSSI Beamer template and mixed it with Pandoc's `default.beamer`.

The YAML front matter should look like this:

~~~yaml
---
title: Pandoc is cool

author: 
    - Albert Einstein
    - Carl Gauss
author-running:
    - A. Einstein
    - C. Gauss
email: 
    - a.einstein@example.com
    - cfg@example.com
institute: 
    - Gran Sasso Science Institute, L'Aquila, Italy
    - Calisota University, Duckburg, USA
institute-running: GSSI
---
~~~

Compile with

    pandoc -t beamer --smart --template=gssi.beamer -o result.pdf source.markdown

(The `--smart` argument is not mandatory, it just looks better)