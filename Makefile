PANDOC = pandoc
DOCSRC = find doc/ -not \( -path ./_build -prune \) -type f -name '*md*' | xargs

.PHONY: all build publish test cover run doc

all: cover doc

build:
	cd Moon && dotnet build && cd ..

publish:
	cd Moon && dotnet publish -c Release && cd ..

test:
	dotnet test

cover:
	dotnet test /p:CollectCoverage=true /p:CoverletOutput=coverage/ /p:CoverletOutputFormat=lcov

run:
	cd Moon && dotnet run -- lex --path $(path) --outdir $(outdir) && cd ..

doc:
	@eval $$(for i in $$($(DOCSRC)); do $(PANDOC) -s $$i -o "$${i%.md}".pdf; done;)

dockerrm:
	docker container rm --force moon

dockerbuild:
	docker build -t moon .

dockerrun:
	# Example: make dockerrun path="~/dev/personal/Moon/Moon.Tests/resources/lexer/in/testcase2.src"
	docker run --rm tatumalenko/moon lex --text "$$(cat $(path))"

dockerclean:
	docker stop $(docker ps -a -q) && docker rm $(docker ps -a -q)

dockerprune:
	docker system prune -a

dockerlistimages:
	docker system prune -a