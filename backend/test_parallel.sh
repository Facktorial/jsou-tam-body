#!/bin/bash
# test_parallel.sh - Test parallel request handling

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Server details
SERVER_URL="http://localhost:8000"
TEST_REGNO="MOV9200" # Replace with actual regno

echo -e "${BLUE}🧪 Testing Parallel Request Processing${NC}"
echo "================================="

# Test 1: Sequential requests (for comparison)
test_sequential() {
	echo -e "${YELLOW}📊 Test 1: Sequential Requests${NC}"
	start_time=$(date +%s%3N)

	echo "Starting events request..."
	curl -s "$SERVER_URL/api/backgroundcheck/$TEST_REGNO/" >/tmp/events.json &
	EVENTS_PID=$!

	echo "Starting standings request..."
	curl -s "$SERVER_URL/api/backgroundcheck/standings/$TEST_REGNO/" >/tmp/standings.json &
	STANDINGS_PID=$!

	# Wait for both to complete
	wait $EVENTS_PID
	wait $STANDINGS_PID

	end_time=$(date +%s%3N)
	duration=$((end_time - start_time))

	echo -e "${GREEN}✅ Sequential requests completed in ${duration}ms${NC}"
	echo ""
}

# Test 2: Parallel requests from same client
test_parallel() {
	echo -e "${YELLOW}🚀 Test 2: Parallel Requests (Same Client)${NC}"
	start_time=$(date +%s%3N)

	# Start both requests simultaneously
	echo "Starting both requests in parallel..."
	curl -s "$SERVER_URL/api/backgroundcheck/$TEST_REGNO/" >/tmp/events_parallel.json &
	EVENTS_PID=$!

	curl -s "$SERVER_URL/api/backgroundcheck/standings/$TEST_REGNO/" >/tmp/standings_parallel.json &
	STANDINGS_PID=$!

	# Wait for both to complete
	wait $EVENTS_PID
	EVENTS_EXIT=$?
	wait $STANDINGS_PID
	STANDINGS_EXIT=$?

	end_time=$(date +%s%3N)
	duration=$((end_time - start_time))

	if [ $EVENTS_EXIT -eq 0 ] && [ $STANDINGS_EXIT -eq 0 ]; then
		echo -e "${GREEN}✅ Parallel requests completed in ${duration}ms${NC}"
	else
		echo -e "${RED}❌ Some requests failed${NC}"
	fi
	echo ""
}

# Test 3: Multiple clients making parallel requests
test_multiple_clients() {
	echo -e "${YELLOW}👥 Test 3: Multiple Clients with Parallel Requests${NC}"
	start_time=$(date +%s%3N)

	# Simulate 3 different clients, each making parallel requests
	for i in {1..3}; do
		(
			echo "Client $i: Starting parallel requests..."
			curl -s "$SERVER_URL/api/backgroundcheck/$TEST_REGNO/" >/tmp/client${i}_events.json &
			curl -s "$SERVER_URL/api/backgroundcheck/standings/$TEST_REGNO/" >/tmp/client${i}_standings.json &
			wait
			echo "Client $i: Completed"
		) &
	done

	# Wait for all clients to complete
	wait

	end_time=$(date +%s%3N)
	duration=$((end_time - start_time))

	echo -e "${GREEN}✅ Multiple clients completed in ${duration}ms${NC}"
	echo ""
}

# Test 4: Server load test
test_server_load() {
	echo -e "${YELLOW}⚡ Test 4: Server Load Test${NC}"
	echo "Making 10 parallel requests to each endpoint..."

	start_time=$(date +%s%3N)

	# Make 10 parallel requests to each endpoint
	for i in {1..10}; do
		curl -s "$SERVER_URL/api/backgroundcheck/$TEST_REGNO/" >/tmp/load_events_$i.json &
		curl -s "$SERVER_URL/api/backgroundcheck/standings/$TEST_REGNO/" >/tmp/load_standings_$i.json &
	done

	# Wait for all requests to complete
	wait

	end_time=$(date +%s%3N)
	duration=$((end_time - start_time))

	echo -e "${GREEN}✅ Load test completed in ${duration}ms (20 total requests)${NC}"
	echo ""
}

# Check if server is running
check_server() {
	if curl -s "$SERVER_URL/api/backgroundcheck/$TEST_REGNO/" >/dev/null 2>&1; then
		echo -e "${GREEN}✅ Server is running${NC}"
		return 0
	else
		echo -e "${RED}❌ Server is not running or not responding${NC}"
		echo "Please start your server first:"
		echo "  stack build && stack exec data-api"
		return 1
	fi
}

# Performance comparison
compare_performance() {
	echo -e "${BLUE}📈 Performance Comparison${NC}"
	echo "================================="

	# Check response times in the JSON files
	if command -v jq &>/dev/null; then
		echo "Events processing time: $(jq -r '.processingTime // "N/A"' /tmp/events.json 2>/dev/null || echo "N/A")"
		echo "Standings processing time: $(jq -r '.processingTime // "N/A"' /tmp/standings.json 2>/dev/null || echo "N/A")"
	else
		echo "Install 'jq' to see detailed processing times"
	fi
	echo ""
}

# Run all tests
main() {
	echo -e "${BLUE}Testing Parallel Request Handling${NC}"
	echo "Server: $SERVER_URL"
	echo "Test Regno: $TEST_REGNO"
	echo "================================="
	echo ""

	# Check if server is running
	if ! check_server; then
		exit 1
	fi

	# Run tests
	test_sequential
	test_parallel
	test_multiple_clients
	test_server_load
	compare_performance

	# Cleanup
	rm -f /tmp/events*.json /tmp/standings*.json /tmp/client*.json /tmp/load_*.json

	echo -e "${GREEN}🎉 All tests completed!${NC}"
}

# Run main function
main "$@"
