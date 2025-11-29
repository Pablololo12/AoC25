DAY ?=

all:
	guile --auto-compile -L lib -L . -L days main.scm $(DAY)
