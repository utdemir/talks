$ echo slides.md | entr -c pandoc -t beamer slides.md -o slides.pdf -V colortheme:beaver -V aspectratio:169
