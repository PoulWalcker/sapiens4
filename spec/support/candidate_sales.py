import csv
import datetime
import io
import json
import re
import sys


def reject(code):
    print(json.dumps({"status": "rejected", "error": code}))
    sys.exit(0)


def date(value):
    if not isinstance(value, str) or not re.fullmatch(r"\d{4}-\d{2}-\d{2}", value):
        reject("invalid_date")
    try:
        return datetime.date.fromisoformat(value)
    except ValueError:
        reject("invalid_date")


request = json.load(sys.stdin)
start, end = date(request["start"]), date(request["end"])
if end < start:
    reject("invalid_period")
rows = csv.DictReader(io.StringIO(request["csv"]))
for name in ("date", "category", "amount_minor"):
    if name not in (rows.fieldnames or []):
        reject("missing_required_column:" + name)
totals, count = {}, 0
for row in rows:
    day = date(row["date"])
    category = row["category"]
    if not category or not category.strip():
        reject("empty_category")
    amount = row["amount_minor"]
    if not isinstance(amount, str) or not re.fullmatch(r"[+-]?\d+", amount):
        reject("invalid_integer:amount_minor")
    if start <= day < end:
        totals[category] = totals.get(category, 0) + int(amount)
        count += 1
print(json.dumps({"status": "ok", "value": {"start": request["start"], "end": request["end"], "row_count": count, "by_category_minor": totals, "total_minor": sum(totals.values())}}))
