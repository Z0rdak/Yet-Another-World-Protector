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
import net.minecraft.command.CommandSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Registry;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.config.server.CommandPermissionConfig.*;
import static de.z0rdak.yawp.core.region.RegionType.DIMENSION;
import static de.z0rdak.yawp.core.region.RegionType.LOCAL;
import static de.z0rdak.yawp.util.MessageUtil.buildRegionInfoLink;
import static de.z0rdak.yawp.util.MessageUtil.sendCmdFeedback;
import static net.minecraft.util.text.TextFormatting.RED;
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
        CommandContextBuilder<CommandSource> cmdContext = event.getParseResults().getContext();
        CommandSource src = cmdContext.getSource();
        List<ParsedCommandNode<CommandSource>> cmdNodes = cmdContext.getNodes();
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
        CommandContextBuilder<CommandSource> cmdContext = event.getParseResults().getContext();
        CommandSource src = cmdContext.getSource();
        List<ParsedCommandNode<CommandSource>> cmdNodes = cmdContext.getNodes();
        List<String> nodeNames = cmdNodes.stream().map(node -> node.getNode().getName()).collect(Collectors.toList());
        // /wp region <dim> <region>
        if (cmdNodes.size() < 4) {
            return;
        }
        // /wp region <dim> <region> info|list|spatial
        if (cmdNodes.size() == 4 || cmdNodes.size() > 4 &&
                (nodeNames.contains(INFO.toString())
                        || nodeNames.contains(LIST.toString())
                        || nodeNames.contains(SPATIAL.toString()))) {
            event.setCanceled(!AllowInfoCmds());
            return;
        }
        // /wp region <dim> <region> state
        if (cmdNodes.size() == 5 && nodeNames.get(4).equals(STATE.toString())) {
            event.setCanceled(!AllowInfoCmds());
            return;
        }
        // check permission for other commands
        ParsedArgument<CommandSource, ?> regionArg = cmdContext.getArguments().get(REGION.toString());
        if (regionArg == null) {
            return;
        }
        if (regionArg.getResult() instanceof IMarkableRegion) {
            IMarkableRegion region = (IMarkableRegion) regionArg.getResult();
            if (src.getEntity() != null) {
                try {
                    if (src.getEntity() instanceof PlayerEntity) {
                        ServerPlayerEntity player = src.getPlayerOrException();
                        boolean hasConfigPermission = hasPlayerPermission(player);
                        // check if player is owner of parent region or has permission to update region area
                        if (cmdNodes.size() > 4 && nodeNames.contains(AREA.toString())) {
                            if (!region.getParent().hasOwner(player.getUUID())
                                    && (player.getTeam() == null || !region.getParent().hasOwner(player.getTeam().getName()))
                                    && !hasConfigPermission) {
                                YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage region '" + region.getName() + "'");
                                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region, LOCAL)));
                                event.setCanceled(true);
                            }
                        }
                        if (!region.hasOwner(player.getUUID())
                                && (player.getTeam() == null || !region.hasOwner(player.getTeam().getName()))
                                && !hasConfigPermission) {
                            YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage region '" + region.getName() + "'");
                            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region, LOCAL)));
                            event.setCanceled(true);
                        }
                    }
                } catch (CommandSyntaxException e) {
                    YetAnotherWorldProtector.LOGGER.error(e);
                }
            } else {
                if (!hasPermission(src)) {
                    YetAnotherWorldProtector.LOGGER.info("' " + src.getTextName() + "' is not allowed to manage region: '" + region.getName() + "' in dim '" + region.getDim().location() + "'!");
                    sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region, LOCAL)));
                    event.setCanceled(true);
                }
            }
        }

    }


    public static void handleDimCommandExecution(CommandEvent event) {
        CommandContextBuilder<CommandSource> cmdContext = event.getParseResults().getContext();
        CommandSource src = cmdContext.getSource();
        List<ParsedCommandNode<CommandSource>> cmdNodes = cmdContext.getNodes();
        List<String> nodeNames = cmdNodes.stream().map(node -> node.getNode().getName()).collect(Collectors.toList());
        if (nodeNames.size() < 3) {
            return;
        }
        if (nodeNames.size() == 3) {
            event.setCanceled(!AllowInfoCmds());
            return;
        }
        if (nodeNames.contains(INFO.toString())
                || nodeNames.contains(LIST.toString())) {
            event.setCanceled(!AllowInfoCmds());
            return;
        }
        // check permission for other commands
        ParsedArgument<CommandSource, ?> dimParsedArgument = cmdContext.getArguments().get(CommandConstants.DIM.toString());
        if (dimParsedArgument.getResult() instanceof ResourceLocation) {
            ResourceLocation dimResLoc = ((ResourceLocation) dimParsedArgument.getResult());
            RegistryKey<World> dim = RegistryKey.create(Registry.DIMENSION_REGISTRY, dimResLoc);
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
            if (dimCache != null) {
                if (src.getEntity() != null) {
                    try {
                        if (src.getEntity() instanceof PlayerEntity) {
                            ServerPlayerEntity player = src.getPlayerOrException();
                            boolean hasConfigPermission = hasPlayerPermission(player);
                            if (!dimCache.hasOwner(player) && !hasConfigPermission) {
                                YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage dim");
                                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.modify.dim.deny", buildRegionInfoLink(dimCache.getDimensionalRegion(), DIMENSION)));
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
                        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.modify.dim.deny", buildRegionInfoLink(dimCache.getDimensionalRegion(), DIMENSION)));
                    }
                }
            } else {
                sendCmdFeedback(src, new StringTextComponent("Dimension not found in region data").withStyle(RED));
            }
        }

    }
}
