a = os.clock()
local s = ""
for i = 1,300000 do
    local a = {}
    a[1] = 1; a[2] = 2; a[3] = 3
end
b = os.clock()
print(b-a)

-- better
a = os.clock()
local s = ""
local t = {}
for i = 1,300000 do
    local a = {0, 0, 0}
    a[1] = 1; a[2] = 2; a[3] = 3
end
s = table.concat(t, "")
b = os.clock()
print(b-a)