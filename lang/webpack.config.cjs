const path = require('path');
const DtsBundleWebpack = require('dts-bundle-webpack')

module.exports = {
  entry: './src/index.ts',
  mode: 'none',
  experiments: {
    outputModule: true,
  },
  resolve: {
    extensionAlias: {
      '.js': ['.js', '.ts'],
    },
  },
  devtool: 'source-map',
  plugins: [
    new DtsBundleWebpack({
      name: "bigdeal-lang",
      main: "build/index.d.ts",
      out: "main.d.ts",
    })
  ],
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
