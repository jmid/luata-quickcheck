f = {}
f.n = 42
f["n"] = function () return 42 end
--f:n().n = 42     -- ok
--f:n()["n"] = 42  -- ok
f.n()
(f)().n = 42     -- not ok
