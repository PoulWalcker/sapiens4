import json
import sys

request = json.load(sys.stdin)


def totals(period):
    result = {}
    for row in period["rows"]:
        category, amount = row["category"], row["amount_minor"]
        if not isinstance(category, str) or not category or type(amount) is not int:
            raise ValueError("Invalid category or integer minor-unit amount")
        result[category] = result.get(category, 0) + amount
    return result


def percentage(current, previous):
    return None if previous == 0 else 100 * (current - previous) / previous


current = totals(request["current"])
previous = totals(request["previous"])
categories = {}
for category in sorted(set(current) | set(previous)):
    left, right = current.get(category, 0), previous.get(category, 0)
    categories[category] = {"current_minor": left, "previous_minor": right,
                            "delta_minor": left - right,
                            "percent_change": percentage(left, right)}
left, right = sum(current.values()), sum(previous.values())
print(json.dumps({"status": "ok", "value": {
    "current_total_minor": left, "previous_total_minor": right,
    "delta_minor": left - right, "percent_change": percentage(left, right),
    "categories": categories}}))
