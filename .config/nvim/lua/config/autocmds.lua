-- Add HTML highlighting for files with .tmpl extension(golang)
vim.api.nvim_create_autocmd("BufWinEnter", {
  pattern = "*.tmpl",
  command = "set filetype=html",
})
