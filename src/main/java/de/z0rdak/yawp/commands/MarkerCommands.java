package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.arguments.region.OwnedRegionArgumentType;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.region.AbstractMarkableRegion;
import de.z0rdak.yawp.core.region.CuboidRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.LiteralTextContent;
import net.minecraft.text.MutableText;
import net.minecraft.text.TranslatableTextContent;

import java.util.Collections;
import java.util.Objects;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.util.CommandUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;
import static de.z0rdak.yawp.util.StickUtil.STICK;
import static de.z0rdak.yawp.util.StickUtil.getStickType;
import static net.minecraft.util.Formatting.RED;

public final class MarkerCommands {

    public static final LiteralArgumentBuilder<ServerCommandSource> MARKER_COMMAND = register();

    private MarkerCommands() {
    }

    private static LiteralArgumentBuilder<ServerCommandSource> register() {
        return literal(MARKER)
                .then(literal(GIVE)
                        .executes(ctx -> giveMarkerStick(ctx.getSource())))
                .then(literal(RESET)
                        .executes(ctx -> resetStick(ctx.getSource())))
                .then(literal(CREATE)
                        .then(CommandManager.argument(REGION.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> CommandSource.suggestMatching(Collections.singletonList("newRegion"), builder))
                                .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), null))
                                .then(CommandManager.argument(PARENT.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> OwnedRegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getParentRegionArgument(ctx))))))
                ;
    }


    // Argument for getting the Marker? Argument could check for player holding valid marker
    private static int createRegion(ServerCommandSource src, String regionName, IMarkableRegion parentRegion) {
        try {
            PlayerEntity player = src.getPlayerOrThrow();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(player.world.getRegistryKey());
            int res = DimensionCommands.checkValidRegionName(regionName, dimCache);
            if (res == -1) {
                sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.name.invalid", regionName)));
                return res;
            }
            if (res == 1) {
                sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), regionName)));
                return res;
            }

            ItemStack maybeStick = player.getMainHandStack();
            if (StickUtil.isVanillaStick(maybeStick)) {
                StickType stickType = StickUtil.getStickType(maybeStick);
                if (stickType == StickType.MARKER) {
                    NbtCompound stickNBT = StickUtil.getStickNBT(maybeStick);
                    if (stickNBT != null) {
                        MarkerStick marker = new MarkerStick(stickNBT);
                        AbstractMarkableRegion region = LocalRegions.regionFrom(player, marker, regionName);
                        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
                        boolean hasConfigPermission = CommandPermissionConfig.hasPlayerPermission(player);
                        if (parentRegion != null) {
                            // should only be a region which has player as owner at this point due to the OwnerRegionArgumentType suggestions
                            if (parentRegion.getOwners().containsPlayer(player.getUuid()) || hasConfigPermission) {
                                if (AbstractMarkableRegion.fullyContains(parentRegion.getArea(), region.getArea())) {
                                    dimCache.addRegion(region);
                                    parentRegion.addChild(region);
                                    LocalRegions.ensureHigherRegionPriorityFor((CuboidRegion) region, RegionConfig.DEFAULT_REGION_PRIORITY.get());
                                    RegionDataManager.save();
                                    sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region))));
                                    return 0;
                                } else {
                                    sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("Parent region does not contain new region")));
                                    return -1;
                                }
                            } else {
                                sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.stick.local.deny", parentRegion.getName())));
                                return 1;
                            }
                        } else {
                            if (dimCache.hasOwner(player) || hasConfigPermission) {
                                dimCache.addRegion(region);
                                LocalRegions.ensureHigherRegionPriorityFor((CuboidRegion) region, RegionConfig.DEFAULT_REGION_PRIORITY.get());
                                RegionDataManager.save();
                                sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region))));
                                return 0;
                            } else {
                                sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.stick.dim.deny", dimCache.getDimensionalRegion().getName())));
                                return 2;
                            }
                        }
                    } else {
                        sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("Invalid marker stick data")));
                        return -2;
                    }
                } else {
                    sendCmdFeedback(src, MutableText.of(new LiteralTextContent(RED + ""))
                            .append(MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.stick.missing"))));
                    return -2;
                }
            } else {
                sendCmdFeedback(src, MutableText.of(new LiteralTextContent(RED + ""))
                        .append(MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.stick.missing"))));
                return -2;
            }
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
            return -3;
        }
    }


    private static int resetStick(ServerCommandSource src) {
        try {
            PlayerEntity player = src.getPlayerOrThrow();
            ItemStack mainHandItem = player.getMainHandStack();
            // is valid stick
            if (!mainHandItem.equals(ItemStack.EMPTY)
                    && StickUtil.hasNonNullTag(mainHandItem)
                    && mainHandItem.getNbt().contains(STICK)) {
                StickType stickType = getStickType(mainHandItem);
                if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                    mainHandItem = StickUtil.initMarkerNbt(mainHandItem, StickType.MARKER, player.getWorld().getRegistryKey());
                    // FIXME: When different area types are available: Get stick, reset it, and save it back.
                    sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("RegionMarker reset!")));
                    return 0;
                } else {
                    sendCmdFeedback(src, MutableText.of(new TranslatableTextContent(RED + "The item in the main hand is not a RegionMarker!")));
                    return 1;
                }
            } else {
                sendCmdFeedback(src, MutableText.of(new TranslatableTextContent(RED + "The item in the main hand is not a RegionMarker!")));
                return 1;
            }
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent(RED + "Command needs a player as command source" + RESET)));
            return 1;
        }
    }

    public static int giveMarkerStick(ServerCommandSource src) {
        try {
            PlayerEntity targetPlayer = src.getPlayerOrThrow();
            ItemStack markerStick = StickUtil.initMarkerNbt(Items.STICK.getDefaultStack(), StickType.MARKER, targetPlayer.world.getRegistryKey());
            targetPlayer.giveItemStack(markerStick);
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("Added RegionMarker to inventory of player '" + targetPlayer.getEntityName() + "'")));
            sendMessage(targetPlayer, MutableText.of(new TranslatableTextContent("RegionMarker added to your inventory.")));
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent(RED + "Command needs a player as command source" + RESET)));
            return 1;
        }
        return 0;
    }
}
