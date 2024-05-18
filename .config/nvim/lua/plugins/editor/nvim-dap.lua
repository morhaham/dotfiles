return {
  {
    "rcarriga/nvim-dap-ui",
    dependencies = {
      "mfussenegger/nvim-dap",
      "nvim-neotest/nvim-nio",
      "folke/neodev.nvim",
    },
    config = function()
      local dap, dapui = require("dap"), require("dapui")
      dap.listeners.before.attach.dapui_config = function()
        dapui.open()
      end
      dap.listeners.before.launch.dapui_config = function()
        dapui.open()
      end
      dap.listeners.before.event_terminated.dapui_config = function()
        dapui.close()
      end
      dap.listeners.before.event_exited.dapui_config = function()
        dapui.close()
      end
      vim.fn.sign_define("DapBreakpoint", { text = "🔴", texthl = "", linehl = "", numhl = "" })
      require("neodev").setup({
        library = { plugins = { "nvim-dap-ui" }, types = true },
      })

      vim.keymap.set({ "n", "v" }, "<M-;>", dapui.eval, { desc = "Evaluate expression" })
      require("dapui").setup()
    end,
  },
  {
    "mfussenegger/nvim-dap",
    config = function()
      local dap = require("dap")

      -- Go
      dap.adapters.go = {
        type = "executable",
        command = "node",
        args = {
          os.getenv("HOME") .. "/dap/vscode-go/extension/dist/debugAdapter.js",
        },
      }
      dap.configurations.go = {
        {
          type = "go",
          name = "Debug",
          request = "launch",
          showLog = false,
          program = "${file}",
          dlvToolPath = vim.fn.exepath("dlv"),
        },
      }

      -- Nodejs
      require("dap").adapters["pwa-node"] = {
        type = "server",
        host = "localhost",
        port = "${port}",
        executable = {
          command = "node",
          args = { "/Users/morh/dap/js-debug/src/dapDebugServer.js", "${port}" },
        },
      }
      require("dap").configurations.typescript = {
        {
          type = "pwa-node",
          request = "attach",
          sourceMaps = true,
          name = "Launch file",
          program = "${file}",
          cwd = "${workspaceFolder}",
        },
      }

      -- keymaps
      vim.keymap.set("n", "<F5>", function()
        require("dap").continue()
      end, { desc = "Continue" })
      vim.keymap.set("n", "<F6>", function()
        require("dap").disconnect()
      end, { desc = "Continue" })
      vim.keymap.set("n", "<F2>", function()
        require("dap").step_over()
      end)
      vim.keymap.set("n", "<F3>", function()
        require("dap").step_into()
      end)
      vim.keymap.set("n", "<F4>", function()
        require("dap").step_out()
      end)
      vim.keymap.set("n", "<Leader>b", function()
        require("dap").toggle_breakpoint()
      end, { desc = "Toggle breakpoint" })
      vim.keymap.set("n", "<Leader>B", function()
        require("dap").set_breakpoint()
      end)
      vim.keymap.set("n", "<Leader>lp", function()
        require("dap").set_breakpoint(nil, nil, vim.fn.input("Log point message: "))
      end)
      vim.keymap.set("n", "<Leader>dr", function()
        require("dap").repl.open()
      end)
      vim.keymap.set("n", "<Leader>dl", function()
        require("dap").run_last()
      end)
      vim.keymap.set({ "n", "v" }, "<Leader>dh", function()
        require("dap.ui.widgets").hover()
      end)
      vim.keymap.set({ "n", "v" }, "<Leader>dp", function()
        require("dap.ui.widgets").preview()
      end)
      vim.keymap.set("n", "<Leader>df", function()
        local widgets = require("dap.ui.widgets")
        widgets.centered_float(widgets.frames)
      end)
      vim.keymap.set("n", "<Leader>ds", function()
        local widgets = require("dap.ui.widgets")
        widgets.centered_float(widgets.scopes)
      end)
    end,
  },
  {
    "theHamsta/nvim-dap-virtual-text",
    dependencies = { "mfussenegger/nvim-dap" },
    config = function()
      require("nvim-dap-virtual-text").setup()
    end,
  },
}
