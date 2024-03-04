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

function Link (x)
  if x.attributes.target == '_blank' then
    x.attributes.target = nil
  end
  if x.content[1].text == 'Mastodon' and x.attributes.onclick ~= nil then
    x.target = 'https://mastodon.social/'
    x.attributes.onclick = nil
  end
  if x.content[1].text == 'Twitter' and string.find(x.target, 'intent') then
    x.target = 'https://twitter.com/'
  end

  return x
end
