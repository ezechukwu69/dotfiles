local M = {
    opts = {
        width = 0.4,
        height = 0.9,
        float_buf = nil,
        float_win = nil,
        set_default_keymaps = true,
    },
    log =
    "jj --no-pager --color never --no-graph -T \"change_id.short()  ++ '	 (' ++ committer.name() ++ ') 	 ' ++ description.first_line() ++ bookmarks.map(|item| ' 	*' ++ item.name() ++ if(item.remote(), '@' ++ item.remote())).join(' ') ++ ' \n'\"",

}

function M.open_float_window_with_keymap(content)
    local calculate_columns = function(columns)
        local width = math.floor(columns * M.opts.width)
        return vim.o.columns - width
    end

    local opts = {
        relative = "editor",
        width = math.floor(vim.o.columns * M.opts.width),
        height = math.floor(vim.o.lines * M.opts.height),
        row = 0,
        col = calculate_columns(vim.o.columns),
        style = "minimal",
        border = "rounded",
    }

    if M.opts.float_buf and vim.api.nvim_buf_is_valid(M.opts.float_buf) then
        if M.opts.float_win and vim.api.nvim_win_is_valid(M.opts.float_win) then
            vim.api.nvim_set_current_win(M.opts.float_win)
            return M.opts.float_buf
        else
            M.opts.float_win = vim.api.nvim_open_win(M.opts.float_buf, true, opts)
            return M.opts.float_buf
        end
    end

    M.opts.float_buf = vim.api.nvim_create_buf(false, true)
    M.opts.float_win = vim.api.nvim_open_win(M.opts.float_buf, true, opts)

    vim.bo[M.opts.float_buf].modifiable = true
    vim.api.nvim_buf_set_lines(M.opts.float_buf, 0, -1, false, content or { "No content" })
    vim.bo[M.opts.float_buf].modifiable = false

    vim.bo[M.opts.float_buf].readonly = true
    vim.wo[M.opts.float_win].wrap = true;
    vim.bo[M.opts.float_buf].buftype = "nofile"
    vim.bo[M.opts.float_buf].bufhidden = "wipe"
    vim.bo[M.opts.float_buf].filetype = "jj"

    vim.keymap.set("n", "q", function()
        if M.opts.float_win and vim.api.nvim_win_is_valid(M.opts.float_win) then
            vim.api.nvim_win_close(M.opts.float_win, true)
        end
    end, { buffer = M.opts.float_buf, nowait = true, silent = true })

    return M.opts.float_buf
end

function M.set_content(buf, content)
    vim.bo[buf].modifiable = true
    vim.bo[buf].readonly = false
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, content)
    vim.cmd("BaleiaColorize")
    vim.bo[buf].modifiable = false
    vim.bo[buf].readonly = true
end

function M.pick_log(on_pick)
    local response = vim.fn.system(M.log)
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

function M.select_remote(on_pick)
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

function M.pick_branch(on_pick)
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

function M.enter_input(prompt, on_enter)
    local input = vim.fn.input(prompt)
    if input and on_enter then
        on_enter(input)
    end
end

function M.jj_st()
    local result = vim.fn.system("jj st")
    local buf = M.open_float_window_with_keymap()
    M.set_content(buf, vim.split(result, "\n"))
end

function M.stringify(value)
    return "\"" .. value .. "\""
end

function M.jj_graph(args)
    local cmd = "jj"
    if args and args ~= "" then
        cmd = cmd .. " " .. M.stringify(args)
    end
    vim.notify(M.stringify(cmd))
    local result = vim.fn.system(cmd)
    local buf = M.open_float_window_with_keymap()
    M.set_content(buf, vim.split(result, "\n"))
end

function M.jj_get_commit_from_log(log)
    local hash = vim.split(log, " ")[1]
    return hash
end

function M.jj_describe()
    M.pick_log(function(item)
        M.enter_input("Describe: ", function(input)
            local commit = "jj describe -m \"" .. input .. "\""
            local result = vim.fn.system(commit)
            local buf = M.open_float_window_with_keymap()
            M.set_content(buf, vim.split(result, "\n"))
        end)
    end)
end

function M.set_branch()
    M.pick_branch(function(item)
        M.pick_log(function(log_line)
            local branch = vim.split(item, ":")[1]
            local response = vim.fn.system("jj bookmark set " .. branch .. " -r " .. M.jj_get_commit_from_log(log_line))
            M.set_content(M.open_float_window_with_keymap(), vim.split(response, "\n"))
        end)
    end)
end

function M.push()
    M.select_remote(function(remote)
        remote = vim.split(remote, " ")[1]
        local response = vim.fn.system("jj git push --remote " .. remote)
        M.set_content(M.open_float_window_with_keymap(), vim.split(response, "\n"))
    end)
end

function M.commit_and_push()
    M.pick_log(function(item)
        local log_local = item
        M.enter_input("Describe: ", function(input)
            local commit = "jj describe -m \"" .. input .. "\""
            local result = vim.fn.system(commit)
            local buf = M.open_float_window_with_keymap()
            M.set_content(buf, vim.split(result, "\n"))
            M.pick_branch(function(branch)
                branch = vim.split(branch, ":")[1]
                local response = vim.fn.system("jj bookmark set " ..
                    branch .. " -r " .. M.jj_get_commit_from_log(log_local))
                M.set_content(M.open_float_window_with_keymap(), vim.split(response, "\n"))
                M.select_remote(function(remote)
                    remote = vim.split(remote, " ")[1]
                    response = vim.fn.system("jj git push --remote " .. remote)
                    M.set_content(M.open_float_window_with_keymap(), vim.split(response, "\n"))
                end)
            end)
        end)
    end)
end

vim.api.nvim_create_user_command("JJ", function(args)
    if args.args == "status" or args.args == "st" then
        M.jj_st()
    elseif args.args == "describe" then
        M.jj_describe()
    elseif args.args == "branch-set" then
        M.set_branch()
    elseif args.args == "push" then
        M.push()
    elseif args.args == "commit-and-push" then
        M.commit_and_push()
    else
        M.jj_graph(args.args)
    end
end, {
    desc = "JJ",
    nargs = "*",
    complete = function()
        return { "commit-and-push", "push", "status", "st", "describe", "branch-set" }
    end,
})

function M.setup(opts)
    M.opts = opts or M.opts
    if M.opts.set_default_keymaps then
        local set = vim.keymap.set
        set("n", "<leader>jjl", function()
            M.jj_graph()
        end, { nowait = true, silent = true, desc = "JJ log" })
        set("n", "<leader>jjs", function()
            M.jj_st()
        end, { nowait = true, silent = true, desc = "JJ status" })
        set("n", "<leader>jjd", function()
            M.jj_describe()
        end, { nowait = true, silent = true, desc = "JJ describe" })
        set("n", "<leader>jjb", function()
            M.set_branch()
        end, { nowait = true, silent = true, desc = "JJ branch" })
        set("n", "<leader>jjp", function()
            M.push()
        end, { nowait = true, silent = true, desc = "JJ push" })
        set("n", "<leader>jjc", function()
            M.commit_and_push()
        end, { nowait = true, silent = true, desc = "JJ commit and push" })
    end
end

return M
