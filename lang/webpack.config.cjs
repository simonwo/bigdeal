const path = require('path');

module.exports = {
  entry: './src/index.ts',
  mode: 'none',
  experiments: {
    outputModule: true,
  },
  output: {
    filename: 'main.js',
    library: {
      type: "module",
    },
    path: path.resolve(__dirname, 'build'),
  },
  module: {
    rules: [
        { test: /\.tsx?$/, use: 'ts-loader', exclude: /node_modules/ },
        { test: /\.ohm$/, use: 'raw-loader' },
        { test: /\.bigdeal$/, use: 'raw-loader' },
    ],
  },
};
