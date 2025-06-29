local function open_float_window_with_keymap(content)
    local buf = vim.api.nvim_create_buf(false, true) -- [listed = false], [scratch = true]
    local win = vim.api.nvim_open_win(buf, true, {
        relative = "editor",
        width = math.floor(vim.o.columns * 0.6),
        height = math.floor(vim.o.lines * 0.5),
        row = math.floor(vim.o.lines * 0.25),
        col = math.floor(vim.o.columns * 0.2),
        style = "minimal",
        border = "rounded",
    })

    -- Make it read-only
    vim.bo[buf].modifiable = false
    vim.bo[buf].readonly = true
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].bufhidden = "wipe"
    vim.bo[buf].filetype = "jj"

    -- Custom keymap: press 'q' to close
    vim.keymap.set("n", "q", function()
        vim.api.nvim_win_close(win, true)
    end, { buffer = buf, nowait = true, silent = true })

    return buf
end

local function set_content(buf, content)
    vim.bo[buf].modifiable = true
    vim.bo[buf].readonly = false
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, content)
    vim.cmd("BaleiaColorize")
    vim.bo[buf].modifiable = false
    vim.bo[buf].readonly = true
end

local log =
"jj --no-pager --color never --no-graph -T \"change_id.short()  ++ '	 (' ++ committer.name() ++ ') 	 ' ++ description.first_line() ++ bookmarks.map(|item| ' 	*' ++ item.name() ++ if(item.remote(), '@' ++ item.remote())).join(' ') ++ ' \n'\""

local function pick_log(on_pick)
    local response = vim.fn.system(
        log
    )
    local lines = vim.split(response, "\n")
    lines = vim.tbl_filter(function(line)
        return line ~= ""
    end, lines)
    vim.ui.select(lines, {
        prompt = "Select a revset: ",
        format_item = function(item)
            return item
        end,
    }, function(item)
        if item and on_pick then
            on_pick(item)
        end
    end)
end


local function select_remote(on_pick)
    local response = vim.fn.system("jj git remote list")
    local lines = vim.split(response, "\n")
    lines = vim.tbl_filter(function(line)
        return line ~= ""
    end, lines)
    vim.ui.select(lines, {
        prompt = "Select a remote: ",
        format_item = function(item)
            return item
        end,
    }, function(item)
        if item and on_pick then
            on_pick(item)
        end
    end)
end

local function pick_branch(on_pick)
    local response = vim.fn.system("jj bookmark list --no-pager --color never")
    local lines = vim.split(response, "\n")
    lines = vim.tbl_filter(function(line)
        return line ~= ""
    end, lines)
    vim.ui.select(lines, {
        prompt = "Select a branch: ",
        format_item = function(item)
            return item
        end,
    }, function(item)
        if item and on_pick then
            on_pick(item)
        end
    end)
end

local function enter_input(prompt, on_enter)
    local input = vim.fn.input(prompt)
    if input and on_enter then
        on_enter(input)
    end
end

local function jj_st()
    local result = vim.fn.system("jj st")
    local buf = open_float_window_with_keymap()
    set_content(buf, vim.split(result, "\n"))
end

local function stringify(value)
    return "\"" .. value .. "\""
end

local function jj_graph(args)
    local cmd = "jj"
    if args and args ~= "" then
        cmd = cmd .. " " .. stringify(args)
    end
    vim.notify(stringify(cmd))
    local result = vim.fn.system(cmd)
    local buf = open_float_window_with_keymap()
    set_content(buf, vim.split(result, "\n"))
end

local function jj_get_commit_from_log(log)
    local hash = vim.split(log, " ")[1]
    return hash
end

local function jj_describe()
    pick_log(function(item)
        enter_input("Describe: ", function(input)
            local commit = "jj describe -m \"" .. input .. "\""
            local result = vim.fn.system(commit)
            local buf = open_float_window_with_keymap()
            set_content(buf, vim.split(result, "\n"))
        end)
    end)
end

local function set_branch()
    pick_branch(function(item)
        pick_log(function(log_line)
            local branch = vim.split(item, ":")[1]
            local response = vim.fn.system("jj bookmark set " .. branch .. " -r " .. jj_get_commit_from_log(log_line))
            set_content(open_float_window_with_keymap(), vim.split(response, "\n"))
        end)
    end)
end

local function push()
    select_remote(function(remote)
        local remote = vim.split(remote, " ")[1]
        local response = vim.fn.system("jj git push --remote " .. remote)
        set_content(open_float_window_with_keymap(), vim.split(response, "\n"))
    end)
end

vim.api.nvim_create_user_command("JJ", function(args)
    if args.args == "status" then
        jj_st()
    elseif args.args == "st" then
        jj_st()
    elseif args.args == "log" then
        pick_log()
    elseif args.args == "describe" then
        jj_describe()
    elseif args.args == "branch-set" then
        set_branch()
    elseif args.args == "push" then
        push()
    else
        jj_graph(args.args)
    end
end, {
    desc = "JJ",
    nargs = "*",
    complete = function()
        return { "push", "status", "st", "log", "describe", "branch-set" }
    end,
})
