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
import net.minecraft.text.Text;
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
        // /wp region <dim> <region> -> Missing region param
        if (!cmdContext.getArguments().containsKey(REGION.toString())) {
            return 0;
        }
        ParsedArgument<ServerCommandSource, ?> regionArg = cmdContext.getArguments().get(REGION.toString());
        if (regionArg != null && regionArg.getResult() instanceof String regionName) {
            ParsedArgument<ServerCommandSource, ?> dimParsedArgument = cmdContext.getArguments().get(DIM.toString());
            if (dimParsedArgument != null && dimParsedArgument.getResult() instanceof Identifier dimResLoc) {
                RegistryKey<World> dim = RegistryKey.of(DefaultedRegistry.WORLD_KEY, dimResLoc);
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
                if (!dimCache.contains(regionName)) {
                    MessageUtil.sendCmdFeedback(cmdContext.getSource(), Text.literal("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().getValue() + "'"));
                    return 1;
                }
                IMarkableRegion region = dimCache.getRegion(regionName);
                try {
                    if (src.getEntity() instanceof PlayerEntity) {
                        ServerPlayerEntity player = src.getPlayerOrThrow();
                        boolean hasConfigPermission = hasPlayerPermission(player);
                        boolean isOwner = region.hasOwner(player.getUuid()) || (player.getScoreboardTeam() != null && region.hasOwner(player.getScoreboardTeam().getName()));
                        boolean isOwnerOfParent = region.getParent() != null && region.getParent().hasOwner(player.getUuid())
                                || (player.getScoreboardTeam() != null && region.getParent().hasOwner(player.getScoreboardTeam().getName()));
                        boolean containsInfoCmd = nodeNames.contains(INFO.toString()) || nodeNames.contains(LIST.toString()) || nodeNames.contains(SPATIAL.toString());

                        // /wp region <dim> <region> info|list|spatial|state
                        if (cmdNodes.size() == 4 || (cmdNodes.size() > 4 && containsInfoCmd) || cmdNodes.size() == 5 && nodeNames.get(4).equals(STATE.toString())) {
                            if (!(isOwner || AllowInfoCmds() || hasConfigPermission)) {
                                sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.region.info.deny", buildRegionInfoLink(region, LOCAL)));
                                return 1;
                            }
                            return 0;
                        }
                        // check if player is owner of parent region or has permission to update region area
                        if (cmdNodes.size() > 4 && nodeNames.contains(AREA.toString())) {
                            if (!isOwnerOfParent && !isOwner && !hasConfigPermission) {
                                YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage region '" + region.getName() + "'");
                                sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region, LOCAL)));
                                return 1;
                            }
                        }
                        // check permission for other commands
                        if (!isOwner && !hasConfigPermission) {
                            YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage region '" + region.getName() + "'");
                            sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region, LOCAL)));
                            return 1;
                        }
                    } else {
                        if (!hasPermission(src)) {
                            YetAnotherWorldProtector.LOGGER.info("' " + src.getName() + "' is not allowed to manage region: '" + region.getName() + "' in dim '" + region.getDim().getValue() + "'!");
                            sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region, LOCAL)));
                            return 1;
                        }
                    }
                } catch (CommandSyntaxException e) {
                    YetAnotherWorldProtector.LOGGER.error(e);
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
        // /wp region <dim> <region> -> Missing dim argument
        if (!cmdContext.getArguments().containsKey(DIM.toString())) {
            return 0;
        }
        ParsedArgument<ServerCommandSource, ?> dimParsedArgument = cmdContext.getArguments().get(DIM.toString());
        if (dimParsedArgument != null && dimParsedArgument.getResult() instanceof Identifier dimResLoc) {
            RegistryKey<World> dim = RegistryKey.of(DefaultedRegistry.WORLD_KEY, dimResLoc);
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
            try {
                if (src.getEntity() instanceof PlayerEntity) {
                    if (dimCache != null) {
                        ServerPlayerEntity player = src.getPlayerOrThrow();
                        boolean hasConfigPermission = hasPlayerPermission(player);
                        boolean isOwner = dimCache.hasOwner(player);

                        // check for info cmd permission
                        boolean isInfoCmd = (nodeNames.size() > 3 && nodeNames.contains(INFO.toString()) || nodeNames.contains(LIST.toString()));
                        if (nodeNames.size() == 3 || isInfoCmd) {
                            if (!(isOwner || AllowInfoCmds() || hasConfigPermission)) {
                                sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.region.info.deny", buildRegionInfoLink(dimCache.getDimensionalRegion(), DIMENSION)));
                                return 1;
                            }
                            return 0;
                        }
                        // check permission for other commands
                        if (!isOwner && !hasConfigPermission) {
                            YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage dim");
                            sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.region.modify.dim.deny", buildRegionInfoLink(dimCache.getDimensionalRegion(), DIMENSION)));
                            return 1;
                        }
                    } else {
                        sendCmdFeedback(src, Text.literal("Dimension not found in region data").formatted(RED));
                        return 0;
                    }
                } else {
                    // server or cmd block?
                    if (!hasPermission(src)) {
                        YetAnotherWorldProtector.LOGGER.info("' " + src.getName() + "' is not allowed to manage dim");
                        sendCmdFeedback(src, Text.translatable("cli.msg.dim.info.region.modify.dim.deny", buildRegionInfoLink(dimCache.getDimensionalRegion(), DIMENSION)));
                        return 1;
                    }
                }
            } catch (CommandSyntaxException e) {
                YetAnotherWorldProtector.LOGGER.error(e);
            }
        }
        return 0;
    }
}
