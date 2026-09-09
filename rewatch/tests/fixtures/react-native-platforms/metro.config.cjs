const { getDefaultConfig, mergeConfig } = require("@react-native/metro-config");

module.exports = mergeConfig(getDefaultConfig(__dirname), {
  // Avoid requiring Watchman for this small, reproducible bundle experiment.
  resolver: { useWatchman: false },
  maxWorkers: 2,
});
