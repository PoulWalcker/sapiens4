import json
import sys

value = json.load(sys.stdin)
records = value["newRecords"]
if any(type(record.get("value")) is not int for record in records):
    print(json.dumps({"status": "rejected", "error": "integer_values_required"}))
else:
    print(json.dumps({"status": "ok", "value": {
        "prefix": value["taskInputs"]["prefix"],
        "records": records,
        "total": sum(record["value"] for record in records)
    }}))
