import json
import sys


def main(value):
    if not isinstance(value, dict) or value.get("interface") != "receipt-feed-input-v1" or value.get("schema") != 1:
        return {"status": "rejected", "error": "unsupported_input"}
    parameters = value.get("taskInputs")
    if not isinstance(parameters, dict) or not isinstance(parameters.get("prefix"), str):
        return {"status": "rejected", "error": "missing_prefix"}
    records = value.get("newRecords")
    if not isinstance(records, list) or any(
        not isinstance(record, dict)
        or not isinstance(record.get("id"), str)
        or type(record.get("value")) is not int
        for record in records
    ):
        return {"status": "rejected", "error": "invalid_record"}
    content = "; ".join(f"{record['id']} = {record['value']}" for record in records)
    text = parameters["prefix"] + ": " + (content if records else "no new records") + "."
    return {"status": "ok", "value": {"text": text}}


print(json.dumps(main(json.load(sys.stdin)), ensure_ascii=False))
