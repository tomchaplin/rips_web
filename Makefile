# Makefile

.PHONY: install dev build serve

default: build package

install:
	lein deps
	npm install

dev:
	sass --watch scss/site.scss:resources/public/css/site.css & \
	lein watch

build:
	sass --sourcemap=none \
		 --style compressed \
		 scss/site.scss resources/public/css/site.css
	lein release

serve:
	http-server resources/public

package:
	rm -f package.zip && \
	cd resources/public && \
		zip -r ../../package.zip * \
		-x js/compiled/manifest.edn \
		js/compiled/cljs-runtime/\* \
		css/\*.map
