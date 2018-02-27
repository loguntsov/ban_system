# BAN-SYSTEM

Application for ban of something by time.
It doesn't keep real data about blocked objects.
It just uses Bloom filters to decide is object blocked or not.
So there is can be collissions for objects.

# USAGE

Start of instance:

```
ban_system:start(blocked_objects, 50000, 5).
```
Where:
 * 50000 - timeout of blocking
 * 5 - count of attempts, after object will be blocked.

Block of object:

```
ban_system:block(blocked_objects, [ object_1 ]).
```

Is object blocked?

```
ban_system:is_blocked(blocked_objects, object_1).
```

Add attempts for object:

```
ban_system:add_attempt(blocked_objects, [ object_1 ]).
```

If attempts are more than 5 then object will be blocked:

```
ban_system:attempts(blocked_objects, [ object_1 ]) =:= [{ object_1, blocked}]
```

After 50000 ms object will be deblocked.

More use cases in bs_test.erl file

# Links

Based on https://github.com/basho/ebloom

# License

MIT