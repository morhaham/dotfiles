-- Disable mouse in Telescope
vim.api.nvim_create_autocmd("User", {
  pattern = "TelescopePreviewerLoaded",
  callback = function()
    vim.opt.mouse = ""
  end
})

-- Re-enable mouse when Telescope is closed
vim.api.nvim_create_autocmd("WinClosed", {
  callback = function()
    vim.opt.mouse = "a"  -- re-enable if you normally use mouse
  end
})
