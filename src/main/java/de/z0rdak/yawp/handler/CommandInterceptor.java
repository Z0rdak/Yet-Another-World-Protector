package de.z0rdak.yawp.handler;

import com.mojang.brigadier.ParseResults;
import com.mojang.brigadier.context.CommandContextBuilder;
import com.mojang.brigadier.context.ParsedArgument;
import com.mojang.brigadier.context.ParsedCommandNode;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.registry.RegistryKey;
import net.minecraft.registry.RegistryKeys;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.text.LiteralTextContent;
import net.minecraft.text.MutableText;
import net.minecraft.util.Identifier;
import net.minecraft.world.World;

import java.util.List;

import static de.z0rdak.yawp.commands.CommandConstants.*;

public class CommandInterceptor {

    /**
     * Handler for managing different command permissions.
     * Sketchy as hell. If CLI format changes, this breaks easily.
     */
    public static int handleModCommands(ParseResults<ServerCommandSource> parseResults, String command) {
        CommandContextBuilder<ServerCommandSource> cmdContext = parseResults.getContext();
        ServerCommandSource src = cmdContext.getSource();
        List<ParsedCommandNode<ServerCommandSource>> cmdNodes = cmdContext.getNodes();
        if (cmdNodes.size() > 2) {
            String baseCmd = cmdNodes.get(0).getNode().getName();
            if (baseCmd.equals(CommandPermissionConfig.BASE_CMD)) {
                YetAnotherWorldProtector.LOGGER.debug("Executed command: '" + parseResults.getReader().getString() + "' by '" + src.getName() + "'.");
                String subCmd = cmdNodes.get(1).getNode().getName();
                switch (subCmd) {
                    case "region":
                        return handleRegionCmdExecution(parseResults, command);
                    case "dim":
                        return handleDimCommandExecution(parseResults, command);
                    case "flag":
                        return 0;
                    case "marker":
                        return handleMarkerCmdExecution(parseResults, command);
                }
            }
        }
        return 0;
    }


    public static int handleMarkerCmdExecution(ParseResults<ServerCommandSource> parseResults, String command) {
        CommandContextBuilder<ServerCommandSource> cmdContext = parseResults.getContext();
        ServerCommandSource src = cmdContext.getSource();
        List<ParsedCommandNode<ServerCommandSource>> cmdNodes = cmdContext.getNodes();
        List<String> nodeNames = cmdNodes.stream().map(node -> node.getNode().getName()).toList();
        if (cmdNodes.size() < 4) {
            return 1;
        }
        // /wp marker create <regionName> without dim
        if (cmdNodes.size() == 4) {
            // TODO: Check if player is allowed to create regions in dim
        }

        // /wp marker create <regionName> <parentName>
        // TODO: Check if player is allowed to create region in parent (local region)
        return 0;
    }

    public static int handleRegionCmdExecution(ParseResults<ServerCommandSource> parseResults, String command) {
        CommandContextBuilder<ServerCommandSource> cmdContext = parseResults.getContext();
        ServerCommandSource src = cmdContext.getSource();
        List<ParsedCommandNode<ServerCommandSource>> cmdNodes = cmdContext.getNodes();
        List<String> nodeNames = cmdNodes.stream().map(node -> node.getNode().getName()).toList();
        if (cmdNodes.size() < 4) {
            return 1;
        }
        // /wp region <dim> <region>
        if (cmdNodes.size() == 4) {
            return 1;
        }
        // /wp region <dim> <region> info|list|spatial
        if (nodeNames.contains(INFO.toString())
                || nodeNames.contains(LIST.toString())
                || nodeNames.contains(SPATIAL.toString())) {
            return 1;
        }
        // /wp region <dim> <region> state
        if (cmdNodes.size() == 5 && nodeNames.get(4).equals(STATE.toString())) {
            return CommandPermissionConfig.AllowInfoCmds() ? 0 : 1;
        }

        // check permission for other commands
        ParsedArgument<ServerCommandSource, ?> dimParsedArgument = cmdContext.getArguments().get(DIMENSION.toString());
        if (dimParsedArgument.getResult() instanceof Identifier dimResLoc) {
            RegistryKey<World> dim = RegistryKey.of(RegistryKeys.WORLD, dimResLoc);
            ParsedArgument<ServerCommandSource, ?> regionArg = cmdContext.getArguments().get(REGION.toString());
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
                                    return 1;
                                }
                            }
                        } catch (CommandSyntaxException e) {
                            YetAnotherWorldProtector.LOGGER.error(e);
                        }
                    } else {
                        if (!CommandPermissionConfig.hasPermission(src)) {
                            YetAnotherWorldProtector.LOGGER.info("' " + src.getName() + "' is not allowed to manage region: '" + region.getName() + "' in dim '" + region.getDim().getValue() + "'!");
                            MessageUtil.sendCmdFeedback(src, MutableText.of(new LiteralTextContent("You are not allowed to manage region: '" + region.getName() + "' in dim '" + region.getDim().getValue() + "'!")));
                            return 1;
                        }
                    }
                } else {
                    MessageUtil.sendCmdFeedback(src, MutableText.of(new LiteralTextContent("Region not found in region data")));
                }
            }
        }
        return 0;
    }


    public static int handleDimCommandExecution(ParseResults<ServerCommandSource> parseResults, String command) {
        CommandContextBuilder<ServerCommandSource> cmdContext = parseResults.getContext();
        ServerCommandSource src = cmdContext.getSource();
        List<ParsedCommandNode<ServerCommandSource>> cmdNodes = cmdContext.getNodes();
        List<String> nodeNames = cmdNodes.stream().map(node -> node.getNode().getName()).toList();

        if (nodeNames.contains(INFO.toString())
                || nodeNames.contains(LIST.toString())) {
            return CommandPermissionConfig.AllowInfoCmds() ? 0 : 1;
        }
        // check permission for other commands
        ParsedArgument<ServerCommandSource, ?> dimParsedArgument = cmdContext.getArguments().get(DIMENSION.toString());
        if (dimParsedArgument.getResult() instanceof Identifier dimResLoc) {
            RegistryKey<World> dim = RegistryKey.of(RegistryKeys.WORLD, dimResLoc);
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
            if (dimCache != null) {
                if (src.getEntity() != null) {
                    try {
                        if (src.getEntity() instanceof PlayerEntity) {
                            ServerPlayerEntity player = src.getPlayerOrThrow();
                            boolean hasConfigPermission = CommandPermissionConfig.hasPlayerPermission(player);
                            if (!dimCache.hasOwner(player) && !hasConfigPermission) {
                                YetAnotherWorldProtector.LOGGER.info("PlayerEntity not allowed to manage dim");
                                MessageUtil.sendCmdFeedback(src, MutableText.of(new LiteralTextContent("You are not allowed to manage this dimensional region!")));
                                return 1;
                            }
                        }
                    } catch (CommandSyntaxException e) {
                        YetAnotherWorldProtector.LOGGER.error(e);
                    }
                } else {
                    if (!CommandPermissionConfig.hasPermission(src)) {
                        YetAnotherWorldProtector.LOGGER.info("' " + src.getName() + "' is not allowed to manage dim");
                        MessageUtil.sendCmdFeedback(src, MutableText.of(new LiteralTextContent("You are not allowed to manage this dimensional region!")));
                        return 1;
                    }
                }
            } else {
                MessageUtil.sendCmdFeedback(src, MutableText.of(new LiteralTextContent("Dimension not found in region data")));
            }
        }
        return 0;
    }


}
