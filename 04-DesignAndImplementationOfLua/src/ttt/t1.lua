a = os.clock()
local s = ""
for i = 1,30000 do
    s = s .. 'a'
end
b = os.clock()
print(b-a)

-- better
a = os.clock()
local s = ""
local t = {}
for i = 1,30000 do
    t[#t + 1] = 'a'
end
s = table.concat(t, "")
b = os.clock()
print(b-a)