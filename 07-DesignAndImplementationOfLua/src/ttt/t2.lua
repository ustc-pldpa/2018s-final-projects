local t = {}

t[1] = 0
t[100] = 0

-- print array
for k,v in ipairs(t) do 
    print(k, v)
end

-- print all
for k,v in pairs(t) do 
    print(k, v)
end