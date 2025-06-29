local M = {}

local float_buf = nil
local float_win = nil

local function open_float_window_with_keymap(content)
    -- Reuse the buffer if it's still valid
    if float_buf and vim.api.nvim_buf_is_valid(float_buf) then
        -- Just reuse buffer and open in a new window
        if float_win and vim.api.nvim_win_is_valid(float_win) then
            vim.api.nvim_set_current_win(float_win)
            return float_buf
        else
            float_win = vim.api.nvim_open_win(float_buf, true, {
                relative = "editor",
                width = math.floor(vim.o.columns * 0.6),
                height = math.floor(vim.o.lines * 0.5),
                row = math.floor(vim.o.lines * 0.25),
                col = math.floor(vim.o.columns * 0.2),
                style = "minimal",
                border = "rounded",
            })
            return float_buf
        end
    end

    -- Otherwise create a new buffer and window
    float_buf = vim.api.nvim_create_buf(false, true)
    float_win = vim.api.nvim_open_win(float_buf, true, {
        relative = "editor",
        width = math.floor(vim.o.columns * 0.6),
        height = math.floor(vim.o.lines * 0.5),
        row = math.floor(vim.o.lines * 0.25),
        col = math.floor(vim.o.columns * 0.2),
        style = "minimal",
        border = "rounded",
    })

    -- Buffer setup
    vim.bo[float_buf].modifiable = true
    vim.api.nvim_buf_set_lines(float_buf, 0, -1, false, content or { "No content" })
    vim.bo[float_buf].modifiable = false

    vim.bo[float_buf].readonly = true
    vim.bo[float_buf].buftype = "nofile"
    vim.bo[float_buf].bufhidden = "wipe"
    vim.bo[float_buf].filetype = "jj"

    -- Keymap to close
    vim.keymap.set("n", "q", function()
        if float_win and vim.api.nvim_win_is_valid(float_win) then
            vim.api.nvim_win_close(float_win, true)
        end
    end, { buffer = float_buf, nowait = true, silent = true })

    return float_buf
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
        remote = vim.split(remote, " ")[1]
        local response = vim.fn.system("jj git push --remote " .. remote)
        set_content(open_float_window_with_keymap(), vim.split(response, "\n"))
    end)
end

local function commit_and_push()
    pick_log(function(item)
        local log_local = item
        enter_input("Describe: ", function(input)
            local commit = "jj describe -m \"" .. input .. "\""
            local result = vim.fn.system(commit)
            local buf = open_float_window_with_keymap()
            set_content(buf, vim.split(result, "\n"))
            pick_branch(function(branch)
                branch = vim.split(branch, ":")[1]
                local response = vim.fn.system("jj bookmark set " ..
                    branch .. " -r " .. jj_get_commit_from_log(log_local))
                set_content(open_float_window_with_keymap(), vim.split(response, "\n"))
                select_remote(function(remote)
                    remote = vim.split(remote, " ")[1]
                    response = vim.fn.system("jj git push --remote " .. remote)
                    set_content(open_float_window_with_keymap(), vim.split(response, "\n"))
                end)
            end)
        end)
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
    elseif args.args == "commit-and-push" then
        commit_and_push()
    else
        jj_graph(args.args)
    end
end, {
    desc = "JJ",
    nargs = "*",
    complete = function()
        return { "commit-and-push", "push", "status", "st", "log", "describe", "branch-set" }
    end,
})
