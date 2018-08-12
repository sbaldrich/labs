#
# This script assumes you've got twurl (https://github.com/twitter/twurl) configured and available in your PATH.
#

import json
from subprocess import run
from sys import stderr
from time import sleep

sleep_on_error_secs = 60 * 15  # Rate limiting window
next_cursor = '-1'

while next_cursor != 0:
    endpoint = '/1.1/followers/list.json?skip_status=true&include_user_entities=false&count=200&cursor={}' \
        .format(next_cursor)
    try:
        process = run(['twurl', endpoint], capture_output=True)
        if process.returncode != 0:
            raise (RuntimeError("Request '{}' did not execute correctly, wait for a bit."))
        output = process.stdout.decode('utf-8')
        data = json.loads(output)
        if data.get('errors', False):
            raise RuntimeError(", ".join(map(lambda err: err['message'], data['errors'])))
        outfile = open("followers_{}.json".format(next_cursor), 'w')
        json.dump(data, outfile)
        next_cursor = data['next_cursor_str']
    except Exception as e:
        print(e, 'Sleeping for {} seconds'.format(sleep_on_error_secs), file=stderr)
        sleep(sleep_on_error_secs)

print("Finished obtaining followers.")
