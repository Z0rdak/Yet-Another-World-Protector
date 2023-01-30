package de.z0rdak.yawp.handler;

import com.mojang.brigadier.context.CommandContextBuilder;
import com.mojang.brigadier.context.ParsedArgument;
import com.mojang.brigadier.context.ParsedCommandNode;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.core.Registry;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.config.server.CommandPermissionConfig.*;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, value = Dist.DEDICATED_SERVER, bus = FORGE)
public class CommandInterceptor {

    /**
     * Handler for managing different command permissions.
     * Sketchy as hell. If CLI format changes, this breaks easily.
     *
     * @param event
     */
    @SubscribeEvent
    public static void handleModCommandPermission(CommandEvent event) {
        CommandContextBuilder<CommandSourceStack> cmdContext = event.getParseResults().getContext();
        CommandSourceStack src = cmdContext.getSource();
        List<ParsedCommandNode<CommandSourceStack>> cmdNodes = cmdContext.getNodes();
        if (cmdNodes.size() > 2) {
            String baseCmd = cmdNodes.get(0).getNode().getName();
            if (baseCmd.equals(WP) || baseCmd.equals(YAWP)) {
                YetAnotherWorldProtector.LOGGER.debug("Executed command: '" + event.getParseResults().getReader().getString() + "' by '" + src.getTextName() + "'.");
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
        CommandContextBuilder<CommandSourceStack> cmdContext = event.getParseResults().getContext();
        CommandSourceStack src = cmdContext.getSource();
        List<ParsedCommandNode<CommandSourceStack>> cmdNodes = cmdContext.getNodes();
        List<String> nodeNames = cmdNodes.stream().map(node -> node.getNode().getName()).collect(Collectors.toList());
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
        CommandContextBuilder<CommandSourceStack> cmdContext = event.getParseResults().getContext();
        CommandSourceStack src = cmdContext.getSource();
        List<ParsedCommandNode<CommandSourceStack>> cmdNodes = cmdContext.getNodes();
        List<String> nodeNames = cmdNodes.stream().map(node -> node.getNode().getName()).collect(Collectors.toList());
        if (cmdNodes.size() < 4) {
            return;
        }
        // /wp region <dim> <region>
        if (cmdNodes.size() == 4) {
            event.setCanceled(!AllowInfoCmds());
            return;
        }
        // /wp region <dim> <region> info|list|spatial
        if (nodeNames.contains(CommandConstants.INFO.toString())
                || nodeNames.contains(CommandConstants.LIST.toString())
                || nodeNames.contains(CommandConstants.SPATIAL.toString())) {
            event.setCanceled(!AllowInfoCmds());
            return;
        }
        // /wp region <dim> <region> state
        if (cmdNodes.size() == 5 && nodeNames.get(4).equals(CommandConstants.STATE.toString())) {
            event.setCanceled(!AllowInfoCmds());
            return;
        }

        // check permission for other commands
        ParsedArgument<CommandSourceStack, ?> dimParsedArgument = cmdContext.getArguments().get(CommandConstants.DIMENSION.toString());
        if (dimParsedArgument.getResult() instanceof ResourceLocation dimResLoc) {
            ResourceKey<Level> dim = ResourceKey.create(Registry.DIMENSION_REGISTRY, dimResLoc);
            ParsedArgument<CommandSourceStack, ?> regionArg = cmdContext.getArguments().get(CommandConstants.REGION.toString());
            if (regionArg.getResult() instanceof String regionName) {
                IMarkableRegion region = RegionDataManager.get().getRegionIn(regionName, dim);
                if (region != null) {

                    if (src.getEntity() != null) {
                        try {
                            if (src.getEntity() instanceof Player) {
                                ServerPlayer player = src.getPlayerOrException();
                                boolean hasConfigPermission = hasPlayerPermission(player);
                                if (!region.getOwners().containsPlayer(player.getUUID()) && !hasConfigPermission) {
                                    YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage dim");
                                    MessageUtil.sendCmdFeedback(src, new TextComponent("You are not allowed to manage this region!"));
                                    event.setCanceled(true);
                                }
                            }
                        } catch (CommandSyntaxException e) {
                            YetAnotherWorldProtector.LOGGER.error(e);
                        }
                    } else {
                        if (!hasPermission(src)) {
                            YetAnotherWorldProtector.LOGGER.info("' " + src.getTextName() + "' is not allowed to manage region: '" + region.getName() + "' in dim '" + region.getDim().location() + "'!");
                            event.setCanceled(true);
                            MessageUtil.sendCmdFeedback(src, new TextComponent("You are not allowed to manage region: '" + region.getName() + "' in dim '" + region.getDim().location() + "'!"));
                        }
                    }
                } else {
                    MessageUtil.sendCmdFeedback(src, new TextComponent("Region not found in region data"));
                }
            }
        }

    }


    public static void handleDimCommandExecution(CommandEvent event) {
        CommandContextBuilder<CommandSourceStack> cmdContext = event.getParseResults().getContext();
        CommandSourceStack src = cmdContext.getSource();
        List<ParsedCommandNode<CommandSourceStack>> cmdNodes = cmdContext.getNodes();
        List<String> nodeNames = cmdNodes.stream().map(node -> node.getNode().getName()).collect(Collectors.toList());

        if (nodeNames.contains(CommandConstants.INFO.toString())
                || nodeNames.contains(CommandConstants.LIST.toString())) {
            event.setCanceled(!AllowInfoCmds());
            return;
        }
        // check permission for other commands
        ParsedArgument<CommandSourceStack, ?> dimParsedArgument = cmdContext.getArguments().get(CommandConstants.DIMENSION.toString());
        if (dimParsedArgument.getResult() instanceof ResourceLocation dimResLoc) {
            ResourceKey<Level> dim = ResourceKey.create(Registry.DIMENSION_REGISTRY, dimResLoc);
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
            if (dimCache != null) {
                if (src.getEntity() != null) {
                    try {
                        if (src.getEntity() instanceof Player) {
                            ServerPlayer player = src.getPlayerOrException();
                            boolean hasConfigPermission = hasPlayerPermission(player);
                            if (!dimCache.hasOwner(player) && !hasConfigPermission) {
                                YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage dim");
                                MessageUtil.sendCmdFeedback(src, new TextComponent("You are not allowed to manage this dimensional region!"));
                                event.setCanceled(true);
                            }
                        }
                    } catch (CommandSyntaxException e) {
                        YetAnotherWorldProtector.LOGGER.error(e);
                    }
                } else {
                    if (!hasPermission(src)) {
                        YetAnotherWorldProtector.LOGGER.info("' " + src.getTextName() + "' is not allowed to manage dim");
                        event.setCanceled(true);
                        MessageUtil.sendCmdFeedback(src, new TextComponent("You are not allowed to manage this dimensional region!"));
                    }
                }
            } else {
                MessageUtil.sendCmdFeedback(src, new TextComponent("Dimension not found in region data"));
            }
        }

    }
}
