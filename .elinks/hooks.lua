-- Written on or around November 17, 2016.

-- From http://stackoverflow.com/a/4991602
function file_exists(name)
    local f = io.open(name, "r")
    if f ~= nil then
        io.close(f)
        return true
    else
        return false
    end
end

function pre_format_html_hook(url, html)
    local timestamp = os.date("%Y-%m-%dT%H-%M-%S%z")
    -- Increment i until we get a fresh location we can write to. This is
    -- necessary not because someone can open multiple pages in the same
    -- second, but rather because ELinks occasionally downloads auxiliary
    -- files, such as CSS files, as it fetches the page.
    local i = 0
    while file_exists(timestamp .. "." .. i .. ".html") do
        i = i + 1
    end
    local name = timestamp .. "." .. i .. ".html"
    -- After obtaining the HTML page, save a copy of it timestamped
    local index = io.open("index.txt", "a")
    index:write(name .. "\t" .. url .. "\n")
    index:close()
    local file = io.open(name, "w")
    file:write(html)
    file:close()
    -- Return the HTML page unmodified
    return nil
end
