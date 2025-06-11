-- Save current mouse setting
local mouse_backup = vim.o.mouse

-- Disable mouse when Telescope starts
vim.api.nvim_create_autocmd("User", {
  pattern = "TelescopeFindPre",
  callback = function()
    mouse_backup = vim.o.mouse
    vim.o.mouse = ""
  end,
})

-- Re-enable mouse when Telescope closes
vim.api.nvim_create_autocmd("User", {
  pattern = "TelescopeFindPost",
  callback = function()
    vim.o.mouse = mouse_backup
  end,
})
