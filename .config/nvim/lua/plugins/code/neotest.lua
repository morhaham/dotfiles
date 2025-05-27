return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "nvim-lua/plenary.nvim",
    "antoinemadec/FixCursorHold.nvim",
    "nvim-treesitter/nvim-treesitter",
    "nvim-neotest/neotest-jest",
  },
  config = function()
    require("neotest").setup({
      discovery = {
        enabled = false,
      },
      adapters = {
        require("neotest-jest")({
          jestCommand = "yarn test",
          jestConfigFile = function(file)
            local path
            if string.find(file, "/apps/") or string.find(file, "/services/") or string.find(file, "/packages/") then
              path = string.match(file, "(.-/[^/]+/)src")
              if path then
                if vim.fn.filereadable(path .. "jest.config.ts") == 1 then
                  return path .. "jest.config.ts"
                elseif vim.fn.filereadable(path .. "jest.config.js") == 1 then
                  return path .. "jest.config.js"
                end
              end
            end
            local cwd = vim.fn.getcwd() .. "/"
            if vim.fn.filereadable(cwd .. "jest.config.ts") == 1 then
              return cwd .. "jest.config.ts"
            elseif vim.fn.filereadable(cwd .. "jest.config.js") == 1 then
              return cwd .. "jest.config.js"
            end
            return nil
          end,
          env = { CI = true },
          cwd = function(file)
            if string.find(file, "/apps/") or string.find(file, "/services/") or string.find(file, "/packages/") then
              return string.match(file, "(.-/[^/]+/)src")
            end
            return vim.fn.getcwd()
          end,
        }),
      },
    })
  end,
}
