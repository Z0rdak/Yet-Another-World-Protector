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
import de.z0rdak.yawp.core.region.RegionType;
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
import net.minecraft.text.Text;

import java.util.Collections;
import java.util.Objects;
import java.util.Random;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.DimensionCommands.regionNameSuggestions;
import static de.z0rdak.yawp.core.region.RegionType.LOCAL;
import static de.z0rdak.yawp.util.CommandUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.buildRegionInfoLink;
import static de.z0rdak.yawp.util.MessageUtil.sendCmdFeedback;
import static de.z0rdak.yawp.util.StickUtil.STICK;
import static de.z0rdak.yawp.util.StickUtil.getStickType;
import static net.minecraft.util.Formatting.RED;


public final class MarkerCommands {

    private MarkerCommands() {
    }

    public static LiteralArgumentBuilder<ServerCommandSource> build() {
        return literal(MARKER)
                .then(literal(GIVE)
                        .executes(ctx -> giveMarkerStick(ctx.getSource())))
                .then(literal(RESET)
                        .executes(ctx -> resetStick(ctx.getSource())))
                .then(literal(CREATE)
                        .then(CommandManager.argument(REGION.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> CommandSource.suggestMatching(Collections.singletonList(regionNameSuggestions.get(new Random().nextInt(regionNameSuggestions.size()))), builder))
                                .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), null))
                                .then(CommandManager.argument(PARENT.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> OwnedRegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getParentRegionArgument(ctx))))))
                ;
    }


    // TODO: Argument for getting the Marker? Argument could check for player holding valid marker
    private static int createRegion(ServerCommandSource src, String regionName, IMarkableRegion parentRegion) {
        try {
            PlayerEntity player = src.getPlayerOrThrow();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(player.getWorld().getRegistryKey());
            int res = DimensionCommands.checkValidRegionName(regionName, dimCache);
            if (res == -1) {
                sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.name.invalid",
                        "Invalid region name supplied: '%s'", regionName));
                return res;
            }
            if (res == 1) {
                sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.name.exists",
                        "Dimension %s already contains region with name %s",
                        buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION), buildRegionInfoLink(dimCache.getRegion(regionName), LOCAL)));
                return res;
            }

            ItemStack maybeStick = player.getMainHandStack();
            if (StickUtil.isVanillaStick(maybeStick)) {
                StickType stickType = StickUtil.getStickType(maybeStick);
                if (stickType == StickType.MARKER) {
                    NbtCompound stickNBT = StickUtil.getStickTag(maybeStick);
                    if (stickNBT != null) {
                        MarkerStick marker = new MarkerStick(stickNBT);
                        if (!marker.isValidArea()) {
                            sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.area.invalid", "Marked area is not valid").formatted(RED));
                            return 1;
                        }
                        AbstractMarkableRegion region = LocalRegions.regionFrom(player, marker, regionName);
                        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
                        boolean hasConfigPermission = CommandPermissionConfig.hasPlayerPermission(player);
                        if (parentRegion != null) {
                            // should only be a region which has player as owner at this point due to the OwnerRegionArgumentType suggestions
                            if (parentRegion.hasOwner(player.getUuid()) || hasConfigPermission) {
                                if (AbstractMarkableRegion.fullyContains(parentRegion.getArea(), region.getArea())) {
                                    dimCache.addRegion(region);
                                    parentRegion.addChild(region);
                                    LocalRegions.ensureHigherRegionPriorityFor((CuboidRegion) region, RegionConfig.DEFAULT_REGION_PRIORITY.get());
                                    RegionDataManager.save();
                                    sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.success",
                                            "Successfully created region %s", buildRegionInfoLink(region, LOCAL)));
                                    return 0;
                                } else {
                                    sendCmdFeedback(src, Text.translatableWithFallback( "cli.msg.dim.info.region.create.stick.area.invalid.parent",
                                            "Parent region %s does not contain the marked area", buildRegionInfoLink(parentRegion, LOCAL)));
                                    return -1;
                                }
                            } else {
                                sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.local.deny",
                                        "You don't have the permission to create a region in the region %s!", buildRegionInfoLink(parentRegion, LOCAL)));
                                return 1;
                            }
                        } else {
                            if (dimCache.hasOwner(player) || hasConfigPermission) {
                                dimCache.addRegion(region);
                                LocalRegions.ensureHigherRegionPriorityFor((CuboidRegion) region, RegionConfig.DEFAULT_REGION_PRIORITY.get());
                                RegionDataManager.save();
                                sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.success",
                                        "Successfully created region %s", buildRegionInfoLink(region, LOCAL)));
                                return 0;
                            } else {
                                sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.dim.deny",
                                        "You don't have the permission to create a region in the dimension %s!",
                                        buildRegionInfoLink(dimCache.getDimensionalRegion(), RegionType.DIMENSION)));
                                return 2;
                            }
                        }
                    } else {
                        sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.invalid",
                                "Invalid RegionMarker data, sorry. Get a new one and try again."));
                        return -2;
                    }
                } else {
                    sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.missing",
                            "Put a valid(*) RegionMarker in your main hand to create a region!").formatted(RED));
                    return -2;
                }
            } else {
                sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.missing",
                        "Put a valid(*) RegionMarker in your main hand to create a region!").formatted(RED));
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
                    sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.reset",
                            "RegionMarker successfully reset!"));
                    return 0;
                } else {
                    sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.missing",
                            "Put a valid(*) RegionMarker in your main hand to create a region!").formatted(RED));
                    return 1;
                }
            } else {
                sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.missing",
                        "Put a valid(*) RegionMarker in your main hand to create a region!").formatted(RED));
                return 1;
            }
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.no-player",
                    "This command can only be executed as a player!").formatted(RED));
            return 1;
        }
    }

    public static int giveMarkerStick(ServerCommandSource src) {
        try {
            PlayerEntity targetPlayer = src.getPlayerOrThrow();
            ItemStack markerStick = StickUtil.initMarkerNbt(Items.STICK.getDefaultStack(), StickType.MARKER, targetPlayer.getWorld().getRegistryKey());
            targetPlayer.giveItemStack(markerStick);
            sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.reset",
                    "RegionMarker successfully reset!"));
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(src, Text.translatableWithFallback("cli.msg.dim.info.region.create.stick.no-player",
                    "This command can only be executed as a player!").formatted(RED));
            return 1;
        }
        return 0;
    }
}
