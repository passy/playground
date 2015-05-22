#!/usr/bin/env python3

import json
import sys
import uuid


def upgrade_tags(f):
    # Get all old tags that need to be upgraded
    tags = list(filter(lambda x: x['id'].startswith('c_'), f['tags']))
    tx = f['transactions']

    for tag in tags:
        new_id = str(uuid.uuid4())

        for t in tx:
            t['tag_ids'] = list(map(lambda x: new_id if x == tag['id'] else x,
                                    t['tag_ids']))

        tag['id'] = new_id

    f.update(tags=tags, transactions=tx)
    return f


def add_missing_tags_for_category(f, category_id, tag_id):
    tx = f['transactions']

    for t in tx:
        if t['category_id'] == category_id and len(t['tag_ids']) == 0:
            set_tag(tag_id)(t)

    f.update(transactions=tx)
    return f


def set_tag(tag_id):
    def _inner(e):
        e['tag_ids'] = [tag_id]
        return e
    return _inner


if __name__ == "__main__":
    with open(sys.argv[1], 'r') as fp:
        f = json.load(fp)
        # fu = upgrade_tags(f)
        fu = add_missing_tags_for_category(
                f,
                category_id='dcc183e8-c417-4c20-8948-04618709d058',
                tag_id='bf2e0837-6d79-421f-b6d4-a17e1c8e334b')
        print(json.dumps(fu, indent=4))
