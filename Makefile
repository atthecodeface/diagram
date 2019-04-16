all: diagram

.PHONY:diagram test client
diagram:
	jbuilder build ./_build/default/src/diagram/diagram_top.exe
	./_build/default/src/diagram/diagram_top.exe --f examples/pipeline.dml examples/pipeline.dss --svg pipeline.svg

client:
	jbuilder build ./_build/default/src/example_client/example_client.exe
	jbuilder build ./_build/default/src/diagram/diagram_top.exe
	(./_build/default/src/diagram/diagram_top.exe & (sleep 1;./_build/default/src/example_client/example_client.exe))

test:
	jbuilder build @run_test

.PHONY:clean
clean:
	jbuilder clean

install:
	jbuilder build @install
	jbuilder install
