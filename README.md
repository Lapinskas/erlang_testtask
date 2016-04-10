# erlang_testtask
Test task for erlang

============
How to check:

1. PREREQUISITES

Erlang and rebar are installed

I'm using my keys for AWS, key will be removed after the tests

DynamoDB has been set us with the table "data" using "key" (string) as primary key and "value" (string) as a field.

2. GET CODE

$> git clone https://github.com/Lapinskas/erlang_testtask.git
$> cd erlang_testtask

3. GET DEPS

$> make deps

4. COMPILE

$> make all

5. LOAD KEYS

$> source priv/access.keys

6. RUN SERVER

$> make server

7. RUN CLIENT
( on another terminal from project folder )

$> make test

8. TEST

8.1 Create record

    1> test_client:test_set("ABC","123").
    Test set: ok
    ok
    
8.2 Get record

    2> test_client:test_get("ABC").      
    Test get: ok
    Test Key: "ABC"
    Test Value: "123"
    ok

8.3 Use wrong key to get record
    
    3> test_client:test_get("Abc").
    Test get: not_found
    ok


============
Status:

Done:	A. Develop a Service 

	B. Create CloudFormation template 
	
	1) The data at rest in DynamoDB is encrypted using AWS KMS service.
	
Done:	2) Data bigger than 4K can be stored.
	
Done:	3) Service exposes an API over TCP using serialized Google protocolbuffer messages as payload.

Done:	4) Rebar (https://github.com/rebar/rebar) is used for building and creating deployments.

Done:	5) Erlcloud (https://github.com/erlcloud/erlcloud.git) is used to access AWS services.
	
Done:	6) A simple test-client is included with server which invokes requests over GPB / TCP interface to verify the Service functionality.

	7) Service is deployed using CloudFormation template into existing VPC network.
	
	
