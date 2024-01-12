package de.z0rdak.yawp.handler;

import com.mojang.brigadier.context.CommandContextBuilder;
import com.mojang.brigadier.context.ParsedArgument;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandSourceType;
import de.z0rdak.yawp.commands.CommandUtil;
import de.z0rdak.yawp.core.region.GlobalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Registry;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.config.server.CommandPermissionConfig.*;
import static de.z0rdak.yawp.util.MessageUtil.buildRegionInfoLink;
import static de.z0rdak.yawp.util.MessageUtil.sendCmdFeedback;
import static net.minecraft.util.text.TextFormatting.RED;
import static net.minecraftforge.fml.common.Mod.EventBusSubscriber.Bus.FORGE;

@Mod.EventBusSubscriber(modid = YetAnotherWorldProtector.MODID, bus = FORGE)
public class CommandInterceptor {

    /**
     * Handler for managing different command permissions.
     */
    @SubscribeEvent
    public static void handleModCommandPermission(CommandEvent event) {
        CommandContextBuilder<CommandSource> cmdContext = event.getParseResults().getContext();
        List<String> nodeNames = cmdContext.getNodes().stream().map(node -> node.getNode().getName()).collect(Collectors.toList());
        CommandSource src = cmdContext.getSource();
        try {
            CommandSourceType cmdSrcType = CommandSourceType.of(src);
            if (!hasModBaseCmd(nodeNames)) {
                return;
            }
            if (nodeNames.size() > 2) {
                YetAnotherWorldProtector.LOGGER.debug("Executed command: '" + event.getParseResults().getReader().getString() + "' by '" + cmdContext.getSource().getTextName() + "'.");
                String subCmd = nodeNames.get(1);
                int cancelExecutionResultCode = 0;
                switch (subCmd) {
                    case "region":
                        if (!cmdContext.getArguments().containsKey(REGION.toString())) {
                            cancelExecutionResultCode = 9;
                            break;
                        }
                        cancelExecutionResultCode = handleRegionCmdExecution(cmdContext, nodeNames, cmdSrcType);
                        break;
                    case "dim":
                        if (!cmdContext.getArguments().containsKey(DIM.toString())) {
                            cancelExecutionResultCode = 9;
                            break;
                        }
                        cancelExecutionResultCode = handleDimCommandExecution(cmdContext, nodeNames, cmdSrcType);
                        break;
                    case "global":
                        if (!cmdContext.getArguments().containsKey(GLOBAL.toString())) {
                            cancelExecutionResultCode = 9;
                            break;
                        }
                        cancelExecutionResultCode = verifyGlobalCommandPermission(cmdContext, nodeNames, cmdSrcType);
                        break;
                    case "flag":
                        if (!cmdContext.getArguments().containsKey(FLAG.toString())) {
                            cancelExecutionResultCode = 9;
                            break;
                        }
                        cancelExecutionResultCode = verifyFlagCommandPermission(cmdContext, nodeNames, cmdSrcType);
                        break;
                    case "marker":
                        if (!cmdContext.getArguments().containsKey(MARKER.toString())) {
                            cancelExecutionResultCode = 9;
                            break;
                        }
                        cancelExecutionResultCode = verifyMarkerCommandPermission(cmdContext, nodeNames, cmdSrcType);
                        break;
                }
                event.setCanceled(cancelExecutionResultCode != 0);
            }

        } catch (IllegalArgumentException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
        }
    }

    private static boolean hasModBaseCmd(List<String> nodeNames) {
        return nodeNames.size() > 0 && nodeNames.get(0) != null && !nodeNames.get(0).equals(BASE_CMD);
    }

    /**
     * TODO: Implement
     *  /wp marker reset|give|create
     */
    private static int verifyMarkerCommandPermission(CommandContextBuilder<CommandSource> cmdContext, List<String> nodeNames, CommandSourceType cmdSrcType) {
        CommandSource src = cmdContext.getSource();
        return 0;

    }


    @Nullable
    private static DimensionRegionCache checkValidDimRegion(CommandContextBuilder<CommandSource> cmdContext) {
        ParsedArgument<CommandSource, ?> dimParsedArgument = cmdContext.getArguments().get(DIM.toString());
        if (dimParsedArgument != null && dimParsedArgument.getResult() instanceof ResourceLocation) {
            ResourceLocation dimResLoc = (ResourceLocation) dimParsedArgument.getResult();
            RegistryKey<World> dim = RegistryKey.create(Registry.DIMENSION_REGISTRY, dimResLoc);
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
            if (dimCache == null) {
                MessageUtil.sendCmdFeedback(cmdContext.getSource(), new StringTextComponent("Dimension not found in region data").withStyle(RED));
                return null;
            }
            return dimCache;
        }
        return null;
    }

    @Nullable
    private static IProtectedRegion checkValidLocalRegion(CommandContextBuilder<CommandSource> cmdContext) {
        ParsedArgument<CommandSource, ?> regionArg = cmdContext.getArguments().get(REGION.toString());
        if (regionArg != null && regionArg.getResult() instanceof String) {
            String regionName = (String) regionArg.getResult();
            ParsedArgument<CommandSource, ?> dimParsedArgument = cmdContext.getArguments().get(DIM.toString());
            if (dimParsedArgument != null && dimParsedArgument.getResult() instanceof ResourceLocation) {
                ResourceLocation dimResLoc = (ResourceLocation) dimParsedArgument.getResult();
                RegistryKey<World> dim = RegistryKey.create(Registry.DIMENSION_REGISTRY, dimResLoc);
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
                if (!dimCache.contains(regionName)) {
                    MessageUtil.sendCmdFeedback(cmdContext.getSource(), new StringTextComponent("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().location() + "'"));
                    return null;
                }
                IMarkableRegion region = dimCache.getRegion(regionName);
                if (region == null) {
                    MessageUtil.sendCmdFeedback(cmdContext.getSource(), new StringTextComponent("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().location() + "'"));
                    return null;
                }
                return region;
            }
        }
        return null;
    }

    private static boolean checkSubCmdAtIndex(List<String> nodeNames, int index, String subCmd, String... subCmds) {
        List<String> subCmdList = Arrays.asList(subCmds);
        subCmdList.add(subCmd);
        return nodeNames.size() >= index + 1 && nodeNames.get(index) != null && nodeNames.stream().anyMatch(subCmdList::contains);
    }

    // TODO: Implement
    private static int verifyFlagCommandPermission(CommandContextBuilder<CommandSource> cmdContext, List<String> nodeNames, CommandSourceType cmdSrcType) {
        CommandSource src = cmdContext.getSource();
        return 0;
    }

    // TODO: Implement
    private static int verifyGlobalCommandPermission(CommandContextBuilder<CommandSource> cmdContext, List<String> nodeNames, CommandSourceType cmdSrcType) {
        CommandSource src = cmdContext.getSource();
        return 0;
    }

    public static int handleRegionCmdExecution(CommandContextBuilder<CommandSource> cmdContext, List<String> nodeNames, CommandSourceType cmdSrcType) {
        CommandSource src = cmdContext.getSource();
        ParsedArgument<CommandSource, ?> regionArg = cmdContext.getArguments().get(REGION.toString());
        if (regionArg != null && regionArg.getResult() instanceof String) {
            String regionName = (String) regionArg.getResult();
            ParsedArgument<CommandSource, ?> dimParsedArgument = cmdContext.getArguments().get(DIM.toString());
            if (dimParsedArgument != null && dimParsedArgument.getResult() instanceof ResourceLocation) {
                ResourceLocation dimResLoc = (ResourceLocation) dimParsedArgument.getResult();
                RegistryKey<World> dim = RegistryKey.create(Registry.DIMENSION_REGISTRY, dimResLoc);
                DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
                if (!dimCache.contains(regionName)) {
                    MessageUtil.sendCmdFeedback(cmdContext.getSource(), new StringTextComponent("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().location() + "'"));
                    return 1;
                }
                IMarkableRegion region = dimCache.getRegion(regionName);
                if (region == null) {
                    MessageUtil.sendCmdFeedback(cmdContext.getSource(), new StringTextComponent("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().location() + "'"));
                    return 1;
                }
                try {
                    if (src.getEntity() instanceof PlayerEntity && region != null) {
                        ServerPlayerEntity player = src.getPlayerOrException();
                        boolean hasConfigPermission = hasPlayerPermission(player);
                        boolean isOwner = region.isInGroup(player, CommandUtil.OWNER);
                        boolean isOwnerOfParent = region.getParent() != null && region.getParent().isInGroup(player, CommandUtil.OWNER);
                        boolean containsInfoCmd = nodeNames.contains(INFO.toString()) || nodeNames.contains(LIST.toString()) || nodeNames.contains(AREA.toString());

                        // /wp region <dim> <region> info|list|spatial|state
                        if (nodeNames.size() == 4 || (nodeNames.size() > 4 && containsInfoCmd) || nodeNames.size() == 5 && nodeNames.get(4).equals(STATE.toString())) {
                            boolean cancelEvent = !(isOwner || AllowInfoCmds() || hasConfigPermission);
                            if (cancelEvent) {
                                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.info.deny", buildRegionInfoLink(region)));
                                return 1;
                            }
                            return 0;
                        }
                        // check if player is owner of parent region or has permission to update region area
                        if (nodeNames.size() > 4 && nodeNames.contains(AREA.toString())) {
                            if (!isOwnerOfParent && !isOwner && !hasConfigPermission) {
                                YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage region '" + region.getName() + "'");
                                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region)));
                                return 1;
                            }
                        }
                        // check permission for other commands
                        if (!isOwner && !hasConfigPermission) {
                            YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage region '" + region.getName() + "'");
                            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region)));
                            return 1;
                        }
                    } else {
                        // server or cmd block
                        if (!hasPermission(src)) {
                            YetAnotherWorldProtector.LOGGER.info("' " + src.getTextName() + "' is not allowed to manage region: '" + region.getName() + "' in dim '" + region.getDim().location() + "'!");
                            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.modify.local.deny", buildRegionInfoLink(region)));
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


    public static int handleDimCommandExecution(CommandContextBuilder<CommandSource> cmdContext, List<String> nodeNames, CommandSourceType cmdSrcType) {
        CommandSource src = cmdContext.getSource();
        ParsedArgument<CommandSource, ?> dimParsedArgument = cmdContext.getArguments().get(DIM.toString());
        if (dimParsedArgument != null && dimParsedArgument.getResult() instanceof ResourceLocation) {
            ResourceLocation dimResLoc = (ResourceLocation) dimParsedArgument.getResult();
            RegistryKey<World> dim = RegistryKey.create(Registry.DIMENSION_REGISTRY, dimResLoc);
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(dim);
            try {
                if (src.getEntity() instanceof PlayerEntity) {
                    if (dimCache != null) {
                        ServerPlayerEntity player = src.getPlayerOrException();
                        boolean hasConfigPermission = hasPlayerPermission(player);
                        boolean isOwner = dimCache.hasOwner(player);
                        // check for info cmd permission
                        boolean isInfoCmd = (nodeNames.size() > 3 && nodeNames.contains(INFO.toString()) || nodeNames.contains(LIST.toString()));
                        if (nodeNames.size() == 3 || isInfoCmd) {
                            boolean cancelEvent = !(isOwner || AllowInfoCmds() || hasConfigPermission);
                            if (cancelEvent) {
                                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.info.deny", buildRegionInfoLink(dimCache.getDimensionalRegion())));
                                return 1;
                            }
                            return 0;
                        }
                        // check permission for other commands
                        if (!isOwner && !hasConfigPermission) {
                            YetAnotherWorldProtector.LOGGER.info("Player not allowed to manage dim");
                            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.modify.dim.deny", buildRegionInfoLink(dimCache.getDimensionalRegion())));
                            return 1;
                        }
                    } else {
                        sendCmdFeedback(src, new StringTextComponent("Dimension not found in region data").withStyle(RED));
                        return 0;
                    }
                } else {
                    // server or cmd block
                    if (!hasPermission(src)) {
                        YetAnotherWorldProtector.LOGGER.info("' " + src.getTextName() + "' is not allowed to manage dim");
                        sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.modify.dim.deny", buildRegionInfoLink(dimCache.getDimensionalRegion())));
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
