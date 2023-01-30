package de.z0rdak.yawp.handler;

public class CommandInterceptor {

    /**
     * Handler for managing different command permissions.
     * Sketchy as hell. If CLI format changes, this breaks easily.
     *
     * @param event

    public static void handleModCommandPermission(CommandEvent event) {
    CommandContextBuilder<ServerCommandSource> cmdContext = event.getParseResults().getContext();
    ServerCommandSource src = cmdContext.getSource();
    List<ParsedCommandNode<ServerCommandSource>> cmdNodes = cmdContext.getNodes();
    if (cmdNodes.size() > 2) {
    String baseCmd = cmdNodes.get(0).getNode().getName();
    if (baseCmd.equals(CommandPermissionConfig.BASE_CMD)) {
    YetAnotherWorldProtector.LOGGER.debug("Executed command: '" + event.getParseResults().getReader().getString() + "' by '" + src.getName() + "'.");
    String subCmd = cmdNodes.get(1).getNode().getName();
    switch (subCmd) {
    case "region":
    handleRegionCmdExecution(event);
    break;
    case "dim":
    handleDimCommandExecution(event);
    break;
    case "flag":
    break;
    case "marker":
    handleMarkerCmdExecution(event);
    break;
    }
    }
    }
    }

    public static void handleMarkerCmdExecution(CommandEvent event) {
    CommandContextBuilder<ServerCommandSource> cmdContext = event.getParseResults().getContext();
    ServerCommandSource src = cmdContext.getSource();
    List<ParsedCommandNode<ServerCommandSource>> cmdNodes = cmdContext.getNodes();
    List<String> nodeNames = cmdNodes.stream().map(node -> node.getNode().getName()).toList();
    if (cmdNodes.size() < 4) {
    return;
    }
    // /wp marker create <regionName> without dim
    if (cmdNodes.size() == 4) {
    // TODO: Check if player is allowed to create regions in dim
    }

    // /wp marker create <regionName> <parentName>
    // TODO: Check if player is allowed to create region in parent (local region)
    }

    public static void handleRegionCmdExecution(CommandEvent event) {
    CommandContextBuilder<ServerCommandSource> cmdContext = event.getParseResults().getContext();
    ServerCommandSource src = cmdContext.getSource();
    List<ParsedCommandNode<ServerCommandSource>> cmdNodes = cmdContext.getNodes();
    List<String> nodeNames = cmdNodes.stream().map(node -> node.getNode().getName()).toList();
    if (cmdNodes.size() < 4) {
    return;
    }
    // /wp region <dim> <region>
    if (cmdNodes.size() == 4) {
    event.setCanceled(!CommandPermissionConfig.AllowInfoCmds());
    return;
    }
    // /wp region <dim> <region> info|list|spatial
    if (nodeNames.contains(CommandConstants.INFO.toString())
    || nodeNames.contains(CommandConstants.LIST.toString())
    || nodeNames.contains(CommandConstants.SPATIAL.toString())) {
    event.setCanceled(!CommandPermissionConfig.AllowInfoCmds());
    return;
    }
    // /wp region <dim> <region> state
    if (cmdNodes.size() == 5 && nodeNames.get(4).equals(CommandConstants.STATE.toString())) {
    event.setCanceled(!CommandPermissionConfig.AllowInfoCmds());
    return;
    }

    // check permission for other commands
    ParsedArgument<ServerCommandSource, ?> dimParsedArgument = cmdContext.getArguments().get(CommandConstants.DIMENSION.toString());
    if (dimParsedArgument.getResult() instanceof Identifier dimResLoc) {
    RegistryKey<World> dim = RegistryKey.of(RegistryKeys.WORLD, dimResLoc);
    ParsedArgument<ServerCommandSource, ?> regionArg = cmdContext.getArguments().get(CommandConstants.REGION.toString());
    if (regionArg.getResult() instanceof String regionName) {
    IMarkableRegion region = RegionDataManager.get().getRegionIn(regionName, dim);
    if (region != null) {

    if (src.getEntity() != null) {
    try {
    if (src.getEntity() instanceof PlayerEntity) {
    ServerPlayerEntity player = src.getPlayerOrThrow();
    boolean hasConfigPermission = CommandPermissionConfig.hasPlayerPermission(player);
    if (!region.getOwners().containsPlayer(player.getUuid()) && !hasConfigPermission) {
    YetAnotherWorldProtector.LOGGER.info("PlayerEntity not allowed to manage dim");
    MessageUtil.sendCmdFeedback(src, MutableText.of(new LiteralTextContent("You are not allowed to manage this region!")));
    event.setCanceled(true);
    }
    }
    } catch (CommandSyntaxException e) {
    YetAnotherWorldProtector.LOGGER.error(e);
    }
    } else {
    if (!CommandPermissionConfig.hasPermission(src)) {
    YetAnotherWorldProtector.LOGGER.info("' " + src.getName() + "' is not allowed to manage region: '" + region.getName() + "' in dim '" + region.getDim().getValue() + "'!");
    event.setCanceled(true);
    MessageUtil.sendCmdFeedback(src, MutableText.of(new LiteralTextContent("You are not allowed to manage region: '" + region.getName() + "' in dim '" + region.getDim().getValue() + "'!")));
    }
    }
    } else {
    MessageUtil.sendCmdFeedback(src, MutableText.of(new LiteralTextContent("Region not found in region data")));
    }
    }
    }

    }


    public static void handleDimCommandExecution(CommandEvent event) {
    CommandContextBuilder<ServerCommandSource> cmdContext = event.getParseResults().getContext();
    ServerCommandSource src = cmdContext.getSource();
    List<ParsedCommandNode<ServerCommandSource>> cmdNodes = cmdContext.getNodes();
    List<String> nodeNames = cmdNodes.stream().map(node -> node.getNode().getName()).collect(Collectors.toList());

    if (nodeNames.contains(CommandConstants.INFO.toString())
    || nodeNames.contains(CommandConstants.LIST.toString())) {
    event.setCanceled(!CommandPermissionConfig.AllowInfoCmds());
    return;
    }
    // check permission for other commands
    ParsedArgument<ServerCommandSource, ?> dimParsedArgument = cmdContext.getArguments().get(CommandConstants.DIMENSION.toString());
    if (dimParsedArgument.getResult() instanceof ResourceLocation dimResLoc) {
    RegistryKey<World> dim = ResourceKey.create(Registry.DIMENSION_REGISTRY, dimResLoc);
    DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
    if (dimCache != null) {
    if (src.getEntity() != null) {
    try {
    if (src.getEntity() instanceof PlayerEntity) {
    ServerPlayerEntity player = src.getPlayerOrException();
    boolean hasConfigPermission = CommandPermissionConfig.hasPlayerPermission(player);
    if (!dimCache.hasOwner(player) && !hasConfigPermission) {
    YetAnotherWorldProtector.LOGGER.info("PlayerEntity not allowed to manage dim");
    MessageUtil.sendCmdFeedback(src, new LiteralTextContent("You are not allowed to manage this dimensional region!"));
    event.setCanceled(true);
    }
    }
    } catch (CommandSyntaxException e) {
    YetAnotherWorldProtector.LOGGER.error(e);
    }
    } else {
    if (!CommandPermissionConfig.hasPermission(src)) {
    YetAnotherWorldProtector.LOGGER.info("' " + src.getTextName() + "' is not allowed to manage dim");
    event.setCanceled(true);
    MessageUtil.sendCmdFeedback(src, new LiteralTextContent("You are not allowed to manage this dimensional region!"));
    }
    }
    } else {
    MessageUtil.sendCmdFeedback(src, new LiteralTextContent("Dimension not found in region data"));
    }
    }

    }
     */
}
