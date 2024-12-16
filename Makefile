doc:
	R -s -e "roxygen2::roxygenize('pkg', load_code = roxygen2::load_pkgload)"

pkg: doc
	rm -f *.tar.gz
	R CMD build pkg

install: pkg
	R CMD INSTALL *.tar.gz

check: pkg
	R CMD check *.tar.gz

cran: pkg
	R CMD check --as-cran *.tar.gz

test: doc
	R -s -e "tinytest::build_install_test('pkg')"

manual: doc
	R CMD Rd2pdf --force -o manual.pdf ./pkg

revdep: pkg
	rm -rf revdep
	mkdir revdep
	mv *.tar.gz revdep
	R -s -e "out <- tools::check_packages_in_dir('revdep',reverse=list(which='most')); print(summary(out)); saveRDS(out, file='revdep/output.RDS')"

site: install
	mkdir -p pkg/sitebuild
	rm -rf pkg/sitebuild/* docs
	cp -r pkg/site/* pkg/sitebuild/
	cd pkg/sitebuild; Rscript -e "litedown::fuse('_footer.Rmd', '.md')"
	cd pkg/sitebuild; Rscript -e "litedown::fuse_site()"
	cd pkg/sitebuild; rm -f *.Rmd *.yml _*
	cp -r pkg/sitebuild docs
	rm -rf pkg/sitebuild
	xdg-open docs/index.html

clean:
	rm -rf *.Rcheck
	rm -rf revdep
	rm -f *.tar.gz
	rm -f manual.pdf
