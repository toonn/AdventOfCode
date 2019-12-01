function Header (x)
  if x.level == 2 then
    return x
  else
    return pandoc.Null
  end
end

function Div (x)
  for _, role in pairs(x.attributes) do
    if role == 'main' then
      local length
      for k, _ in pairs(x.content) do
        length = k
      end
      return {table.unpack(x.content, 1, length - 3)}
    end
  end
  return pandoc.Null
end

function Para (x)
  return x
end
