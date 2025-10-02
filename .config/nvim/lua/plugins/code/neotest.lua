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
          jestCommand = "node node_modules/.bin/jest",
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
            -- if string.find(file, "/apps/") or string.find(file, "/services/") or string.find(file, "/packages/") then
            --   return string.match(file, "(.-/[^/]+/)src")
            -- end
            return vim.fn.getcwd()
          end,
        }),
      },
    })

    vim.keymap.set("n", "<leader>tn", function()
      require("neotest").run.run()
    end, { desc = "Run nearest test" })
    vim.keymap.set("n", "<leader>tf", function()
      require("neotest").run.run(vim.fn.expand("%"))
    end, { desc = "Run tests in file" })
    vim.keymap.set("n", "<leader>ts", function()
      require("neotest").run.stop()
    end, { desc = "Stop running tests" })
    vim.keymap.set("n", "<leader>to", function()
      require("neotest").output.open({ enter = true })
    end, { desc = "Open test output" })
    vim.keymap.set("n", "<leader>tO", function()
      require("neotest").output_panel.toggle()
    end, { desc = "Toggle test output panel" })
    vim.keymap.set("n", "<leader>tS", function()
      require("neotest").summary.toggle()
    end, { desc = "Toggle test summary" })
    vim.keymap.set("n", "<leader>tw", function()
      require("neotest-jest")({
        jestCommand = require("neotest-jest.jest-util").getJestCommand(vim.fn.expand("%:p:h")) .. " --watch",
      })
    end, { desc = "Run tests in watch mode" })
  end,
}
