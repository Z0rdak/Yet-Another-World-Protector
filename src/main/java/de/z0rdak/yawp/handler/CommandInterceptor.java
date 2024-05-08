package de.z0rdak.yawp.handler;

import com.mojang.brigadier.context.CommandContextBuilder;
import com.mojang.brigadier.context.ParsedArgument;
import com.mojang.brigadier.context.ParsedCommandNode;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.core.registries.Registries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.bus.api.Event;
import net.neoforged.bus.api.EventPriority;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.fml.common.Mod;
import net.neoforged.neoforge.common.ToolActions;
import net.neoforged.neoforge.event.CommandEvent;
import net.neoforged.neoforge.event.ServerChatEvent;
import net.neoforged.neoforge.event.entity.EntityJoinLevelEvent;
import net.neoforged.neoforge.event.entity.EntityMobGriefingEvent;
import net.neoforged.neoforge.event.entity.EntityMountEvent;
import net.neoforged.neoforge.event.entity.EntityTeleportEvent;
import net.neoforged.neoforge.event.entity.item.ItemTossEvent;
import net.neoforged.neoforge.event.entity.living.*;
import net.neoforged.neoforge.event.entity.player.*;
import net.neoforged.neoforge.event.level.*;
import net.neoforged.neoforge.registries.NeoForgeRegistries;

import static de.z0rdak.yawp.util.MessageUtil.sendCmdFeedback;

import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.config.server.CommandPermissionConfig.*;
import static de.z0rdak.yawp.core.region.RegionType.DIMENSION;
import static de.z0rdak.yawp.core.region.RegionType.LOCAL;
import static de.z0rdak.yawp.util.MessageUtil.buildRegionInfoLink;
import static net.minecraft.ChatFormatting.RED;

@EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER, bus = EventBusSubscriber.Bus.GAME)
public class CommandInterceptor {

    /**
     * Handler for managing different command permissions.
     */
    @SubscribeEvent
    public static void handleModCommandPermission(CommandEvent event) {
        CommandContextBuilder<CommandSourceStack> cmdContext = event.getParseResults().getContext();
        CommandSourceStack src = cmdContext.getSource();
        List<ParsedCommandNode<CommandSourceStack>> cmdNodes = cmdContext.getNodes();
        if (cmdNodes.size() > 2) {
            String baseCmd = cmdNodes.get(0).getNode().getName();
            if (baseCmd.equals(BASE_CMD)) {
                YetAnotherWorldProtector.LOGGER.debug("Executed command: '" + event.getParseResults().getReader().getString() + "' by '" + src.getTextName() + "'.");
                String subCmd = cmdNodes.get(1).getNode().getName();
                switch (subCmd) {
                    case "region":
                        handleRegionCmdExecution(event);
                        break;
                    case "dim":
                        handleDimCommandExecution(event);
                        break;
                }
            }
        }
    }

    public static void handleRegionCmdExecution(CommandEvent event) {
        CommandContextBuilder<CommandSourceStack> cmdContext = event.getParseResults().getContext();
        CommandSourceStack src = cmdContext.getSource();
        List<ParsedCommandNode<CommandSourceStack>> cmdNodes = cmdContext.getNodes();
        List<String> nodeNames = cmdNodes.stream().map(node -> node.getNode().getName()).collect(Collectors.toList());
        // /wp region <dim> <region> -> Missing region param
        if (!cmdContext.getArguments().containsKey(REGION.toString())) {
            return;
        }
        ParsedArgument<CommandSourceStack, ?> regionArg = cmdContext.getArguments().get(REGION.toString());
        if (regionArg != null && regionArg.getResult() instanceof String regionName) {
            ParsedArgument<CommandSourceStack, ?> dimParsedArgument = cmdContext.getArguments().get(DIM.toString());
            if (dimParsedArgument != null && dimParsedArgument.getResult() instanceof ResourceLocation dimResLoc) {
                ResourceKey<Level> dim = ResourceKey.create(Registries.DIMENSION, dimResLoc);
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
                if (!dimCache.contains(regionName)) {
                    sendCmdFeedback(cmdContext.getSource(), Component.literal("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().location() + "'"));
                    event.setCanceled(true);
                    return;
                }
                IMarkableRegion region = dimCache.getRegion(regionName);
                try {
                    if (src.getEntity() instanceof Player) {
                        ServerPlayer player = src.getPlayerOrException();
                        boolean hasConfigPermission = hasPlayerPermission(player);
                        boolean isOwner = region.hasOwner(player.getUUID()) || (player.getTeam() != null && region.hasOwner(player.getTeam().getName()));
                        boolean isOwnerOfParent = region.getParent() != null && region.getParent().hasOwner(player.getUUID())
                                || (player.getTeam() != null && region.getParent().hasOwner(player.getTeam().getName()));
                        boolean containsInfoCmd = nodeNames.contains(INFO.toString()) || nodeNames.contains(LIST.toString()) || nodeNames.contains(SPATIAL.toString());

                        // /wp region <dim> <region> info|list|spatial|state
                        if (cmdNodes.size() == 4 || (cmdNodes.size() > 4 && containsInfoCmd) || cmdNodes.size() == 5 && nodeNames.get(4).equals(STATE.toString())) {
                            event.setCanceled(!(isOwner || AllowInfoCmds() || hasConfigPermission));
                            if (event.isCanceled()) {
                                sendCmdFeedback(src, Component.translatableWithFallback("cli.msg.dim.info.region.info.deny", "You don't have permission to view the region information for %s!",buildRegionInfoLink(region, LOCAL)));
                            }
                            return;
                        }
                        // check if player is owner of parent region or has permission to update region area
                        if (cmdNodes.size() > 4 && nodeNames.contains(AREA.toString())) {
                            if (!isOwnerOfParent && !isOwner && !hasConfigPermission) {
                                YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage region '" + region.getName() + "'");
                                sendCmdFeedback(src, Component.translatableWithFallback("cli.msg.dim.info.region.modify.local.deny", "You don't have the permission to modify the local region %s!", buildRegionInfoLink(region, LOCAL)));
                                event.setCanceled(true);
                            }
                        }
                        // check permission for other commands
                        if (!isOwner && !hasConfigPermission) {
                            YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage region '" + region.getName() + "'");
                            sendCmdFeedback(src, Component.translatableWithFallback("cli.msg.dim.info.region.modify.local.deny", "You don't have the permission to modify the local region %s!", buildRegionInfoLink(region, LOCAL)));
                            event.setCanceled(true);
                        }
                    } else {
                        if (!hasPermission(src)) {
                            YetAnotherWorldProtector.LOGGER.info("' " + src.getTextName() + "' is not allowed to manage region: '" + region.getName() + "' in dim '" + region.getDim().location() + "'!");
                            sendCmdFeedback(src, Component.translatableWithFallback("cli.msg.dim.info.region.modify.local.deny", "You don't have the permission to modify the local region %s!", buildRegionInfoLink(region, LOCAL)));
                            event.setCanceled(true);
                        }
                    }
                } catch (CommandSyntaxException e) {
                    YetAnotherWorldProtector.LOGGER.error(e);
                }
            }
        }
    }

    public static void handleDimCommandExecution(CommandEvent event) {
        CommandContextBuilder<CommandSourceStack> cmdContext = event.getParseResults().getContext();
        CommandSourceStack src = cmdContext.getSource();
        List<ParsedCommandNode<CommandSourceStack>> cmdNodes = cmdContext.getNodes();
        List<String> nodeNames = cmdNodes.stream().map(node -> node.getNode().getName()).collect(Collectors.toList());
        // /wp region <dim> <region> -> Missing dim argument
        if (!cmdContext.getArguments().containsKey(DIM.toString())) {
            return;
        }
        ParsedArgument<CommandSourceStack, ?> dimParsedArgument = cmdContext.getArguments().get(DIM.toString());
        if (dimParsedArgument != null && dimParsedArgument.getResult() instanceof ResourceLocation dimResLoc) {
            ResourceKey<Level> dim = ResourceKey.create(Registries.DIMENSION, dimResLoc);
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
            try {
                if (src.getEntity() instanceof Player) {
                    if (dimCache != null) {
                        ServerPlayer player = src.getPlayerOrException();
                        boolean hasConfigPermission = hasPlayerPermission(player);
                        boolean isOwner = dimCache.hasOwner(player);

                        // check for info cmd permission
                        boolean isInfoCmd = (nodeNames.size() > 3 && nodeNames.contains(INFO.toString()) || nodeNames.contains(LIST.toString()));
                        if (nodeNames.size() == 3 || isInfoCmd) {
                            event.setCanceled(!(isOwner || AllowInfoCmds() || hasConfigPermission));
                            if (event.isCanceled()) {
                                sendCmdFeedback(src, Component.translatableWithFallback("cli.msg.dim.info.region.info.deny", "You don't have permission to view the region information for %s!", buildRegionInfoLink(dimCache.getDimensionalRegion(), DIMENSION)));
                            }
                            return;
                        }
                        // check permission for other commands
                        if (!isOwner && !hasConfigPermission) {
                            YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage dim");
                            sendCmdFeedback(src, Component.translatableWithFallback("cli.msg.dim.info.region.modify.dim.deny", "You don't have the permission to modify the dimensional region %s!", buildRegionInfoLink(dimCache.getDimensionalRegion(), DIMENSION)));
                            event.setCanceled(true);
                        }
                    } else {
                        sendCmdFeedback(src, Component.literal("Dimension not found in region data").withStyle(RED));
                    }
                } else {
                    // server or cmd block?
                    if (!hasPermission(src)) {
                        YetAnotherWorldProtector.LOGGER.info("' " + src.getTextName() + "' is not allowed to manage dim");
                        event.setCanceled(true);
                        sendCmdFeedback(src, Component.translatableWithFallback("cli.msg.dim.info.region.modify.dim.deny", "You don't have the permission to modify the dimensional region %s!", buildRegionInfoLink(dimCache.getDimensionalRegion(), DIMENSION)));
                    }
                }
            } catch (CommandSyntaxException e) {
                YetAnotherWorldProtector.LOGGER.error(e);
            }
        }
    }
}
