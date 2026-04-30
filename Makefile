# h/t to the package Makefiles in ~/Rtinycc
PKGNAME := $(shell sed -n 's/Package: *\([^ ]*\)/\1/p' DESCRIPTION)
PKGVERS := $(shell sed -n 's/Version: *\([^ ]*\)/\1/p' DESCRIPTION)

.PHONY: all rd test install build check rdm clean

all: check

rd:
	Rscript -e 'roxygen2::roxygenize(load_code = "source")'

test:
	Rscript -e 'tinytest::test_package("$(PKGNAME)")'

install:
	R CMD INSTALL --preclean .

build:
	R CMD build .

check: build
	R CMD check --as-cran --no-manual $(PKGNAME)_$(PKGVERS).tar.gz

rdm: install
	Rscript -e "rmarkdown::render('README.Rmd', quiet = FALSE)"

clean:
	rm -rf $(PKGNAME).Rcheck
	rm -f $(PKGNAME)_*.tar.gz
