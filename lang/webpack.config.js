const path = require('path');

module.exports = {
  entry: './src/index.ts',
  mode: 'none',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'build'),
  },
  module: {
    rules: [
        { test: /\.ohm$/, use: 'raw-loader' },
        { test: /\.bigdeal$/, use: 'raw-loader' },
    ],
  },
};
