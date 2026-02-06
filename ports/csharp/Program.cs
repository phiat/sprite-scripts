using System;
using System.CommandLine;
using SpriteTool.Commands;

// ============================================================
// Root command
// ============================================================
var rootCommand = new RootCommand("A CLI tool for managing sprites with coding agents");

// ============================================================
// launch subcommand
// ============================================================
var launchCommand = new Command("launch", "Create and configure a sprite with coding agent, git, beads");

var dryRunOption = new Option<bool>("--dry-run", "Show what would happen without executing");
var noCheckpointOption = new Option<bool>("--no-checkpoint", "Disable auto-checkpointing");
var uploadOption = new Option<string[]>("--upload", "Upload a local directory to /home/sprite/<dirname> (repeatable)")
{
    AllowMultipleArgumentsPerToken = false,
};
var spriteNameArg = new Argument<string>("sprite-name", "Name of the sprite to create/use");
var planFileArg = new Argument<string?>("plan-file", () => null, "Path to plan file (optional)");

launchCommand.AddOption(dryRunOption);
launchCommand.AddOption(noCheckpointOption);
launchCommand.AddOption(uploadOption);
launchCommand.AddArgument(spriteNameArg);
launchCommand.AddArgument(planFileArg);

launchCommand.SetHandler((dryRun, noCheckpoint, uploads, spriteName, planFile) =>
{
    var exitCode = LaunchCommand.Execute(dryRun, noCheckpoint, uploads ?? [], spriteName, planFile);
    Environment.Exit(exitCode);
}, dryRunOption, noCheckpointOption, uploadOption, spriteNameArg, planFileArg);

rootCommand.AddCommand(launchCommand);

// ============================================================
// push subcommand
// ============================================================
var pushCommand = new Command("push", "Push local file or directory to a sprite");

var pushLocalArg = new Argument<string>("local-path", "Local file or directory path");
var pushRemoteArg = new Argument<string>("remote-path", "Remote path on sprite");
var pushSpriteArg = new Argument<string?>("sprite-name", () => null, "Sprite name (optional, uses default)");

pushCommand.AddArgument(pushLocalArg);
pushCommand.AddArgument(pushRemoteArg);
pushCommand.AddArgument(pushSpriteArg);

pushCommand.SetHandler((localPath, remotePath, spriteName) =>
{
    var exitCode = PushCommand.Execute(localPath, remotePath, spriteName);
    Environment.Exit(exitCode);
}, pushLocalArg, pushRemoteArg, pushSpriteArg);

rootCommand.AddCommand(pushCommand);

// ============================================================
// pull subcommand
// ============================================================
var pullCommand = new Command("pull", "Pull file or directory from a sprite");

var pullRemoteArg = new Argument<string>("remote-path", "Remote path on sprite");
var pullLocalArg = new Argument<string>("local-path", "Local file or directory path");
var pullSpriteArg = new Argument<string?>("sprite-name", () => null, "Sprite name (optional, uses default)");

pullCommand.AddArgument(pullRemoteArg);
pullCommand.AddArgument(pullLocalArg);
pullCommand.AddArgument(pullSpriteArg);

pullCommand.SetHandler((remotePath, localPath, spriteName) =>
{
    var exitCode = PullCommand.Execute(remotePath, localPath, spriteName);
    Environment.Exit(exitCode);
}, pullRemoteArg, pullLocalArg, pullSpriteArg);

rootCommand.AddCommand(pullCommand);

// ============================================================
// watch subcommand
// ============================================================
var watchCommand = new Command("watch", "Poll a sprite's beads tracker task for progress");

var watchSpriteArg = new Argument<string>("sprite-name", "Name of the sprite to watch");
var watchTaskArg = new Argument<string?>("task-id", () => null, "Beads task ID to track (default: auto-detect)");
var watchIntervalArg = new Argument<int>("poll-interval", () => 30, "Seconds between polls (default: 30)");

watchCommand.AddArgument(watchSpriteArg);
watchCommand.AddArgument(watchTaskArg);
watchCommand.AddArgument(watchIntervalArg);

watchCommand.SetHandler((spriteName, taskId, pollInterval) =>
{
    var exitCode = WatchCommand.Execute(spriteName, taskId, pollInterval);
    Environment.Exit(exitCode);
}, watchSpriteArg, watchTaskArg, watchIntervalArg);

rootCommand.AddCommand(watchCommand);

// ============================================================
// Run
// ============================================================
return rootCommand.Invoke(args);
