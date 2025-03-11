local ns_id = vim.api.nvim_create_namespace("inline_diff")
local extmarks = {} -- Store diff positions
local showing_inline_diff = false

-- Function to jump to next diff
local function jump_to_next_diff()
  local cursor = vim.api.nvim_win_get_cursor(0)
  local current_line = cursor[1] - 1 -- Convert to 0-based indexing

  for _, mark in ipairs(extmarks) do
    if mark[1] > current_line then
      vim.api.nvim_win_set_cursor(0, { mark[1] + 1, 0 }) -- Convert back to 1-based
      return
    end
  end

  vim.notify("No more diffs below", vim.log.levels.INFO)
end

local function jump_to_previous_diff()
  local cursor = vim.api.nvim_win_get_cursor(0)
  local current_line = cursor[1] - 1 -- Convert to 0-based indexing

  for i = #extmarks, 1, -1 do
    if extmarks[i][1] < current_line then
      vim.api.nvim_win_set_cursor(0, { extmarks[i][1] + 1, 0 }) -- Convert back to 1-based
      return
    end
  end

  vim.notify("No more diffs above", vim.log.levels.INFO)
end

local update_keymaps = function()
  if showing_inline_diff then
    vim.keymap.set("n", "]o", jump_to_next_diff, { desc = "Jump to next diff", buffer = true })
    vim.keymap.set("n", "[o", jump_to_previous_diff, { desc = "Jump to previous diff", buffer = true })
    vim.keymap.set("n", "<esc><esc>", function()
      GDiffClear()
    end, { desc = "Disable gdiff", buffer = true })
  else
    vim.keymap.del("n", "]o", { buffer = true })
    vim.keymap.del("n", "[o", { buffer = true })
    vim.keymap.del("n", "<esc><esc>", { buffer = true })
  end
end

local function get_commit_list(file)
  local cmd = "git --no-pager log --diff-filter=AM --pretty=format:'%h %s' --follow -- " .. vim.fn.shellescape(file)
  local commits = vim.fn.systemlist(cmd)

  if #commits == 0 then
    vim.notify("No commit history found for this file", vim.log.levels.WARN)
    return nil
  end

  -- Convert to table of {hash, message}
  local choices = {}
  for _, commit in ipairs(commits) do
    local hash, msg = commit:match("^(%S+) (.+)")
    if hash and msg then
      table.insert(choices, { hash = hash, msg = msg })
    end
  end

  return choices
end

-- Function to show inline diffs
local function show_inline_diff(commit_hash, file)
  -- Clear previous virtual lines
  vim.api.nvim_buf_clear_namespace(0, ns_id, 0, -1)
  extmarks = {}

  -- Get the diff output for this file
  local diff_cmd = "git diff --unified=0 " .. commit_hash .. " -- " .. vim.fn.shellescape(file)
  local diff_output = vim.fn.systemlist(diff_cmd)

  if #diff_output == 0 then
    vim.notify("No changes in this commit.", vim.log.levels.INFO)
    return
  end

  -- Process the diff output
  local line_num = nil
  for _, line in ipairs(diff_output) do
    -- Extract line numbers from diff chunks
    local start_line = line:match("^@@ %-%d+,%d* %+?(%d+),?%d* @@")
    if start_line then
      line_num = tonumber(start_line) - 1 -- Convert to 0-based indexing
    elseif line_num and line:sub(1, 1) == "+" then
      -- Display added lines in virtual text
      local mark = vim.api.nvim_buf_set_extmark(0, ns_id, line_num, 0, {
        virt_lines = { { { "│ " .. line, "DiffAdd" } } },
        virt_lines_above = false, -- Show below the line
      })
      table.insert(extmarks, { line_num, mark })
    elseif line_num and line:sub(1, 1) == "-" then
      -- Display removed lines above the affected line
      local mark = vim.api.nvim_buf_set_extmark(0, ns_id, line_num, 0, {
        virt_lines = { { { "│ " .. line, "DiffDelete" } } },
        virt_lines_above = true, -- Show above the line
      })
      table.insert(extmarks, { line_num, mark })
    end
  end
  showing_inline_diff = true
  update_keymaps()
  vim.notify("Inline diff added for commit " .. commit_hash, vim.log.levels.INFO)
end

-- Main function to run GDiff
function GDiffRun()
  local file = vim.fn.expand("%")

  -- Get commit history for the file
  local commits = get_commit_list(file)
  if not commits then
    return
  end

  -- Format commit list for display
  local choices = {}
  for _, commit in ipairs(commits) do
    table.insert(choices, commit.hash .. " - " .. commit.msg)
  end

  -- Use vim.ui.select for better selection
  vim.ui.select(choices, {
    prompt = "Select a commit:",
  }, function(choice)
    if not choice then
      vim.notify("No commit selected", vim.log.levels.WARN)
      return
    end

    -- Extract selected commit hash
    local commit_hash = choice:match("^(%S+) ")
    if commit_hash then
      show_inline_diff(commit_hash, file)
    end
  end)
end

function GDiffClear()
  if showing_inline_diff then
    vim.api.nvim_buf_clear_namespace(0, ns_id, 0, -1)
    showing_inline_diff = false
    update_keymaps()
    print("Cleared inline diff")
  end
end

vim.api.nvim_create_user_command("GDiff", function(opts_one)
  GDiffRun()
end, { nargs = "?" }) -- Run with ":GDiff <commit_hash>"

vim.api.nvim_create_user_command("GDiffClear", function()
  GDiffClear()
end, {})
