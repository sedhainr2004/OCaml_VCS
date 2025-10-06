Author: Jared Viani (jcv75)
Author: Anthony Ahn (aja282)
Author: Naveen Ramasamy (nr453)
Author: Rohan Sedhain (rs2443)
Author: Akul Maheshwari (am2737)

Collaborators: None

Libaries Used:

- ocaml-fileutils: https://github.com/gildor478/ocaml-fileutils
- Unix: https://ocaml.org/manual/5.2/api/Unix.html

Sources Used:

- The Unix module in OCaml does not provide built-in support for generating diffs between files, so we used a couple of webpages to
  figure out how we could implement our own diff feature.

  1.  https://medium.com/@gabrielschade/how-git-diff-works-a-sample-with-f-af3e3737963
  2.  https://stackoverflow.com/questions/51027181/git-how-does-git-know-the-changes-of-a-text-file
  3.  https://github.com/git/git/blob/master/diff.c
  4.  https://florian.github.io/diffing/

- Our ID generation algorithm is based on the twitter snowflake algorithm, so we used some articles and repos to help us out

  5.  https://blog.devtrovert.com/p/how-to-generate-unique-ids-in-distributed
  6.  https://github.com/jtejido/snowflake
  7.  https://abheist.com/blogs/twitter-snowflake-for-unique-ids
