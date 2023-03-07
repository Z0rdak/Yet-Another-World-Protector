package de.z0rdak.yawp.handler;

import com.mojang.brigadier.ParseResults;
import com.mojang.brigadier.context.CommandContextBuilder;
import com.mojang.brigadier.context.ParsedArgument;
import com.mojang.brigadier.context.ParsedCommandNode;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.text.LiteralText;
import net.minecraft.text.TranslatableText;
import net.minecraft.util.Identifier;
import net.minecraft.util.registry.DefaultedRegistry;
import net.minecraft.util.registry.RegistryKey;
import net.minecraft.world.World;

import java.util.List;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.config.server.CommandPermissionConfig.*;
import static de.z0rdak.yawp.core.region.RegionType.DIMENSION;
import static de.z0rdak.yawp.core.region.RegionType.LOCAL;
import static de.z0rdak.yawp.util.MessageUtil.buildRegionInfoLink;
import static de.z0rdak.yawp.util.MessageUtil.sendCmdFeedback;
import static net.minecraft.util.Formatting.RED;

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
            if (baseCmd.equals(BASE_CMD)) {
                YetAnotherWorldProtector.LOGGER.debug("Executed command: '" + parseResults.getReader().getString() + "' by '" + src.getName() + "'.");
                String subCmd = cmdNodes.get(1).getNode().getName();
                switch (subCmd) {
                    case "region":
                        return handleRegionCmdExecution(parseResults, command);
                    case "dim":
                        return handleDimCommandExecution(parseResults, command);
                    default:
                        return 0;
                }
            }
        }
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
            return AllowInfoCmds() ? 0 : 1;
        }
        // /wp region <dim> <region> info|list|spatial
        if (nodeNames.contains(INFO.toString())
                || nodeNames.contains(LIST.toString())
                || nodeNames.contains(SPATIAL.toString())) {
            return AllowInfoCmds() ? 0 : 1;
        }
        // /wp region <dim> <region> state
        if (cmdNodes.size() == 5 && nodeNames.get(4).equals(STATE.toString())) {
            return AllowInfoCmds() ? 0 : 1;
        }

        // check permission for other commands
        ParsedArgument<ServerCommandSource, ?> dimParsedArgument = cmdContext.getArguments().get(DIM.toString());
        if (dimParsedArgument.getResult() instanceof Identifier dimResLoc) {
            RegistryKey<World> dim = RegistryKey.of(DefaultedRegistry.WORLD_KEY, dimResLoc);
            ParsedArgument<ServerCommandSource, ?> regionArg = cmdContext.getArguments().get(REGION.toString());
            if (regionArg == null) {
                return 1;
            }
            if (regionArg.getResult() instanceof IMarkableRegion region) {
                if (src.getEntity() != null) {
                    try {
                        if (src.getEntity() instanceof PlayerEntity) {
                            ServerPlayerEntity player = src.getPlayer();
                            boolean hasConfigPermission = hasPlayerPermission(player);
                            // check if player is owner of parent region or has permission to update region area
                            if (cmdNodes.size() > 4 && nodeNames.contains(AREA.toString())) {
                                // TODO: method to check if player is owner (player or team)
                                if (!region.getParent().hasOwner(player.getUuid())
                                        && (player.getScoreboardTeam() == null || !region.getParent().hasOwner(player.getScoreboardTeam().getName()))
                                        && !hasConfigPermission) {
                                    YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage region '" + region.getName() + "'");
                                    sendCmdFeedback(src, new TranslatableText("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region, LOCAL)));
                                    return 1;
                                }
                            }
                            // TODO: method to check if player is owner (player or team)
                            if (!region.hasOwner(player.getUuid())
                                    && (player.getScoreboardTeam() == null || !region.hasOwner(player.getScoreboardTeam().getName()))
                                    && !hasConfigPermission) {
                                YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage region '" + region.getName() + "'");
                                sendCmdFeedback(src, new TranslatableText("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region, LOCAL)));
                                return 1;
                            }
                        }
                    } catch (CommandSyntaxException e) {
                        YetAnotherWorldProtector.LOGGER.error(e);
                    }
                } else {
                    if (!hasPermission(src)) {
                        YetAnotherWorldProtector.LOGGER.info("' " + src.getName() + "' is not allowed to manage region: '" + region.getName() + "' in dim '" + region.getDim().getValue() + "'!");
                        sendCmdFeedback(src, new TranslatableText("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region, LOCAL)));
                        return 1;
                    }
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
        if (nodeNames.size() < 3) {
            return 1;
        }
        if (nodeNames.size() == 3) {
            return AllowInfoCmds() ? 0 : 1;
        }
        if (nodeNames.contains(INFO.toString())
                || nodeNames.contains(LIST.toString())) {
            return AllowInfoCmds() ? 0 : 1;
        }
        // check permission for other commands
        ParsedArgument<ServerCommandSource, ?> dimParsedArgument = cmdContext.getArguments().get(DIM.toString());
        if (dimParsedArgument.getResult() instanceof Identifier dimResLoc) {
            RegistryKey<World> dim = RegistryKey.of(DefaultedRegistry.WORLD_KEY, dimResLoc);
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
            if (dimCache != null) {
                if (src.getEntity() != null) {
                    try {
                        if (src.getEntity() instanceof PlayerEntity) {
                            ServerPlayerEntity player = src.getPlayer();
                            boolean hasConfigPermission = hasPlayerPermission(player);
                            if (!dimCache.hasOwner(player) && !hasConfigPermission) {
                                YetAnotherWorldProtector.LOGGER.info("PlayerEntity not allowed to manage dim");
                                sendCmdFeedback(src, new TranslatableText("cli.msg.dim.info.region.modify.dim.deny", buildRegionInfoLink(dimCache.getDimensionalRegion(), DIMENSION)));
                                return 1;
                            }
                        }
                    } catch (CommandSyntaxException e) {
                        YetAnotherWorldProtector.LOGGER.error(e);
                    }
                } else {
                    if (!hasPermission(src)) {
                        YetAnotherWorldProtector.LOGGER.info("' " + src.getName() + "' is not allowed to manage dim");
                        sendCmdFeedback(src, new TranslatableText("cli.msg.dim.info.region.modify.dim.deny", buildRegionInfoLink(dimCache.getDimensionalRegion(), DIMENSION)));
                        return 1;
                    }
                }
            } else {
                MessageUtil.sendCmdFeedback(src, new LiteralText("Dimension not found in region data").formatted(RED));
            }
        }
        return 0;
    }


}
