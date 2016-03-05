GEN=java -jar ${HOME}/.m2/repository/io/swagger/swagger-codegen-cli/2.1.6-SNAPSHOT/swagger-codegen-cli-2.1.6-SNAPSHOT.jar
API_SPEC=http://data.deutschebahn.com/apis/fahrplan/db-fahrplan-api-specification.yaml

gen:
	${GEN} generate -i ${API_SPEC} -l haskell-servant -o . --api-package Web.Bahn --model-package Web.Bahn.Model --invoker-package Web.Bahn

build:
	stack build

help:
	${GEN} help generate
