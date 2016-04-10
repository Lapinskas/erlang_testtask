# erlang_testtask
Test task for erlang

Status:

Done:	A. Develop a Service 
	B. Create CloudFormation template 
	1) The data at rest in DynamoDB is encrypted using AWS KMS service.
	2) Data bigger than 4K can be stored.
Done:	3) Service exposes an API over TCP using serialized Google protocolbuffer messages as payload.
Done:	4) Rebar (https://github.com/rebar/rebar) is used for building and creating deployments.
	5) Erlcloud (https://github.com/erlcloud/erlcloud.git) is used to access AWS services.
Done:	6) A simple test-client is included with server which invokes requests over GPB / TCP interface to verify the Service functionality.
	7) Service is deployed using CloudFormation template into existing VPC network.