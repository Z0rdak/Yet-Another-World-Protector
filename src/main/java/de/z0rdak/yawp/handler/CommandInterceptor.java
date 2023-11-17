package de.z0rdak.yawp.handler;

import com.mojang.brigadier.context.CommandContextBuilder;
import com.mojang.brigadier.context.ParsedArgument;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandUtil;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.core.Registry;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.config.server.CommandPermissionConfig.*;
import static de.z0rdak.yawp.util.MessageUtil.buildRegionInfoLink;
import static de.z0rdak.yawp.util.MessageUtil.sendCmdFeedback;
import static net.minecraft.ChatFormatting.RED;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, bus = FORGE)
public class CommandInterceptor {

    /**
     * Handler for managing different command permissions.
     */
    @SubscribeEvent
    public static void handleModCommandPermission(CommandEvent event) {
        CommandContextBuilder<CommandSourceStack> cmdContext = event.getParseResults().getContext();
        List<String> nodeNames = cmdContext.getNodes().stream().map(node -> node.getNode().getName()).collect(Collectors.toList());
        if (nodeNames.size() > 0 && nodeNames.get(0) != null && !nodeNames.get(0).equals(BASE_CMD)) {
            return;
        }
        if (nodeNames.size() > 2) {
            YetAnotherWorldProtector.LOGGER.debug("Executed command: '" + event.getParseResults().getReader().getString() + "' by '" + cmdContext.getSource().getTextName() + "'.");
            String subCmd = nodeNames.get(1);
            int cancelExecutionResultCode = 0;
            switch (subCmd) {
                case "region":
                    cancelExecutionResultCode = handleRegionCmdExecution(cmdContext, nodeNames);
                    break;
                case "dim":
                    cancelExecutionResultCode = handleDimCommandExecution(cmdContext, nodeNames);
                    break;
                case "global":
                    cancelExecutionResultCode = verifyGlobalCommandPermission(cmdContext, nodeNames);
                    break;
                case "flag":
                    cancelExecutionResultCode = verifyFlagCommandPermission(cmdContext, nodeNames);
                    break;
                case "marker":
                    cancelExecutionResultCode = verifyMarkerCommandPermission(cmdContext, nodeNames);
                    break;
            }
            event.setCanceled(cancelExecutionResultCode != 0);
        }

    }

    /**
     * TODO: Implement
     *  /wp marker reset|give|create
     */
    private static int verifyMarkerCommandPermission(CommandContextBuilder<CommandSourceStack> cmdContext, List<String> nodeNames) {
        CommandSourceStack src = cmdContext.getSource();
        return 0;
    }

    // TODO: Implement
    private static int verifyFlagCommandPermission(CommandContextBuilder<CommandSourceStack> cmdContext, List<String> nodeNames) {
        CommandSourceStack src = cmdContext.getSource();
        return 0;
    }

    // TODO: Implement
    private static int verifyGlobalCommandPermission(CommandContextBuilder<CommandSourceStack> cmdContext, List<String> nodeNames) {
        CommandSourceStack src = cmdContext.getSource();
        return 0;
    }

    public static int handleRegionCmdExecution(CommandContextBuilder<CommandSourceStack> cmdContext, List<String> nodeNames) {
        CommandSourceStack src = cmdContext.getSource();
        // /wp region <dim> <region> -> Missing region param
        if (!cmdContext.getArguments().containsKey(REGION.toString())) {
            return 0;
        }
        ParsedArgument<CommandSourceStack, ?> regionArg = cmdContext.getArguments().get(REGION.toString());
        if (regionArg != null && regionArg.getResult() instanceof String regionName) {
            ParsedArgument<CommandSourceStack, ?> dimParsedArgument = cmdContext.getArguments().get(DIM.toString());
            if (dimParsedArgument != null && dimParsedArgument.getResult() instanceof ResourceLocation dimResLoc) {
                ResourceKey<Level> dim = ResourceKey.create(Registry.DIMENSION_REGISTRY, dimResLoc);
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
                if (!dimCache.contains(regionName)) {
                    MessageUtil.sendCmdFeedback(cmdContext.getSource(), new TextComponent("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().location() + "'"));
                    return 1;
                }
                IMarkableRegion region = dimCache.getRegion(regionName);
                try {
                    if (src.getEntity() instanceof Player && region != null) {
                        ServerPlayer player = src.getPlayerOrException();
                        boolean hasConfigPermission = hasPlayerPermission(player);
                        boolean isOwner = region.isInGroup(player, CommandUtil.OWNER);
                        boolean isOwnerOfParent = region.getParent() != null && region.getParent().isInGroup(player, CommandUtil.OWNER);
                        boolean containsInfoCmd = nodeNames.contains(INFO.toString()) || nodeNames.contains(LIST.toString()) || nodeNames.contains(AREA.toString());

                        // /wp region <dim> <region> info|list|spatial|state
                        if (nodeNames.size() == 4 || (nodeNames.size() > 4 && containsInfoCmd) || nodeNames.size() == 5 && nodeNames.get(4).equals(STATE.toString())) {
                            boolean cancelEvent = !(isOwner || AllowInfoCmds() || hasConfigPermission);
                            if (cancelEvent) {
                                sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.region.info.deny", buildRegionInfoLink(region)));
                                return 1;
                            }
                            return 0;
                        }
                        // check if player is owner of parent region or has permission to update region area
                        if (nodeNames.size() > 4 && nodeNames.contains(AREA.toString())) {
                            if (!isOwnerOfParent && isOwner && !hasConfigPermission) {
                                YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage region '" + region.getName() + "'");
                                sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region)));
                                return 1;
                            }
                        }
                        // check permission for other commands
                        if (!isOwner && !hasConfigPermission) {
                            YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage region '" + region.getName() + "'");
                            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region)));
                            return 1;
                        }
                    } else {
                        if (!hasPermission(src)) {
                            YetAnotherWorldProtector.LOGGER.info("' " + src.getTextName() + "' is not allowed to manage region: '" + region.getName() + "' in dim '" + region.getDim().location() + "'!");
                            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region)));
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

    public static int handleDimCommandExecution(CommandContextBuilder<CommandSourceStack> cmdContext, List<String> nodeNames) {
        CommandSourceStack src = cmdContext.getSource();
        // /wp region <dim> <region> -> Missing dim argument
        if (!cmdContext.getArguments().containsKey(DIM.toString())) {
            return 0;
        }
        ParsedArgument<CommandSourceStack, ?> dimParsedArgument = cmdContext.getArguments().get(DIM.toString());
        if (dimParsedArgument != null && dimParsedArgument.getResult() instanceof ResourceLocation dimResLoc) {
            ResourceKey<Level> dim = ResourceKey.create(Registry.DIMENSION_REGISTRY, dimResLoc);
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
                            boolean cancelEvent = !(isOwner || AllowInfoCmds() || hasConfigPermission);
                            if (cancelEvent) {
                                sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.region.info.deny", buildRegionInfoLink(dimCache.getDimensionalRegion())));
                                return 1;
                            }
                            return 0;
                        }
                        // check permission for other commands
                        if (!isOwner && !hasConfigPermission) {
                            YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage dim");
                            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.region.modify.dim.deny", buildRegionInfoLink(dimCache.getDimensionalRegion())));
                            return 1;
                        }
                    } else {
                        sendCmdFeedback(src, new TextComponent("Dimension not found in region data").withStyle(RED));
                        return 0;
                    }
                } else {
                    if (!hasPermission(src)) {
                        YetAnotherWorldProtector.LOGGER.info("' " + src.getTextName() + "' is not allowed to manage dim");
                        sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.region.modify.dim.deny", buildRegionInfoLink(dimCache.getDimensionalRegion())));
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
