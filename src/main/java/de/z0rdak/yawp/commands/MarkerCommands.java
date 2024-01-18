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
import net.minecraft.command.Commands;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.text.TranslationTextComponent;

import java.util.Collections;
import java.util.Objects;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.commands.DimensionCommands.getRandomExample;
import static de.z0rdak.yawp.commands.arguments.ArgumentUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.buildRegionInfoLink;
import static de.z0rdak.yawp.util.MessageUtil.sendCmdFeedback;
import static de.z0rdak.yawp.util.StickUtil.STICK;
import static de.z0rdak.yawp.util.StickUtil.getStickType;
import static net.minecraft.util.text.TextFormatting.RED;

public final class MarkerCommands {

    private MarkerCommands() {
    }

    public static LiteralArgumentBuilder<CommandSource> build() {
        return literal(MARKER)
                .then(literal(GIVE)
                        .executes(ctx -> giveMarkerStick(ctx.getSource())))
                .then(literal(RESET)
                        .executes(ctx -> resetStick(ctx.getSource())))
                .then(literal(CREATE)
                        .then(Commands.argument(CommandConstants.LOCAL.toString(), StringArgumentType.word())
                                .suggests((ctx, builder) -> ISuggestionProvider.suggest(Collections.singletonList(getRandomExample()), builder))
                                .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), null))
                                .then(Commands.argument(PARENT.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> OwnedRegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getParentRegionArgument(ctx))))))
                ;
    }

    private static int createRegion(CommandSource src, String regionName, IMarkableRegion parentRegion) {
        try {
            PlayerEntity player = src.getPlayerOrException();
            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(player.level.dimension());
            int res = DimensionCommands.checkValidRegionName(regionName, dimCache);
            if (res == -1) {
                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.name.invalid", regionName));
                return res;
            }
            if (res == 1) {
                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.name.exists", buildRegionInfoLink(dimCache.getDimensionalRegion()), buildRegionInfoLink(dimCache.getRegion(regionName))));
                return res;
            }


            ItemStack maybeStick = player.getMainHandItem();
            if (StickUtil.isVanillaStick(maybeStick)) {
                StickType stickType = StickUtil.getStickType(maybeStick);
                if (stickType == StickType.MARKER) {
                    CompoundNBT stickNBT = StickUtil.getStickNBT(maybeStick);
                    if (stickNBT != null) {
                        MarkerStick marker = new MarkerStick(stickNBT);
                        if (!marker.isValidArea()) {
                            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.stick.area.invalid").withStyle(RED));
                            return 1;
                        }
                        AbstractMarkableRegion region = LocalRegions.regionFrom(player, marker, regionName);
                        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
                        boolean hasConfigPermission = CommandPermissionConfig.hasPlayerPermission(player);
                        if (parentRegion != null) {
                            // should only be a region which has player as owner at this point due to the OwnerRegionArgumentType suggestions
                            if (parentRegion.hasPlayer(player.getUUID(), CommandUtil.OWNER) || hasConfigPermission) {
                                if (AbstractMarkableRegion.fullyContains(parentRegion.getArea(), region.getArea())) {
                                    dimCache.addRegion(region);
                                    parentRegion.addChild(region);
                                    LocalRegions.ensureHigherRegionPriorityFor((CuboidRegion) region, RegionConfig.getDefaultPriority());
                                    RegionDataManager.save();
                                    sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region)));
                                    return 0;
                                } else {
                                    sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.stick.area.invalid.parent", buildRegionInfoLink(parentRegion)));
                                    return -1;
                                }
                            } else {
                                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.local.deny", buildRegionInfoLink(parentRegion)));
                                return 1;
                            }
                        } else {
                            if (dimCache.hasOwner(player) || hasConfigPermission) {
                                dimCache.addRegion(region);
                                LocalRegions.ensureHigherRegionPriorityFor((CuboidRegion) region, RegionConfig.getDefaultPriority());
                                RegionDataManager.save();
                                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region)));
                                return 0;
                            } else {
                                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.dim.deny", buildRegionInfoLink(dimCache.getDimensionalRegion())));
                                return 2;
                            }
                        }
                    } else {
                        sendCmdFeedback(src, new TranslationTextComponent(  "cli.msg.dim.info.region.create.stick.invalid"));
                        return -2;
                    }
                } else {
                    sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.stick.missing").withStyle(RED));
                    return -2;
                }
            } else {
                sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.stick.missing").withStyle(RED));
                return -2;
            }
        } catch (CommandSyntaxException e) {
            YetAnotherWorldProtector.LOGGER.error(e);
            return -3;
        }
    }


    private static int resetStick(CommandSource src) {
        try {
            PlayerEntity player = src.getPlayerOrException();
            ItemStack mainHandItem = player.getMainHandItem();
            // is valid stick
            if (!mainHandItem.equals(ItemStack.EMPTY)
                    && StickUtil.hasNonNullTag(mainHandItem)
                    && mainHandItem.getTag().contains(STICK)) {
                StickType stickType = getStickType(mainHandItem);
                if (Objects.requireNonNull(stickType) == StickType.MARKER) {
                    mainHandItem = StickUtil.initMarkerNbt(mainHandItem, StickType.MARKER, player.getCommandSenderWorld().dimension());
                    // Note: When different area types are available: Get stick, reset it, and save it back.
                    sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.stick.reset"));
                    return 0;
                } else {
                    sendCmdFeedback(src, new TranslationTextComponent( "cli.msg.dim.info.region.create.stick.missing").withStyle(RED));
                    return 1;
                }
            } else {
                sendCmdFeedback(src, new TranslationTextComponent( "cli.msg.dim.info.region.create.stick.missing").withStyle(RED));
                return 1;
            }
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(src, new TranslationTextComponent(  "cli.msg.dim.info.region.create.stick.no-player").withStyle(RED));
            return 1;
        }
    }

    public static int giveMarkerStick(CommandSource src) {
        try {
            PlayerEntity targetPlayer = src.getPlayerOrException();
            ItemStack markerStick = StickUtil.initMarkerNbt(Items.STICK.getDefaultInstance(), StickType.MARKER, targetPlayer.level.dimension());
            targetPlayer.addItem(markerStick);
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.stick.success"));
        } catch (CommandSyntaxException e) {
            sendCmdFeedback(src, new TranslationTextComponent("cli.msg.dim.info.region.create.stick.no-player").withStyle(RED));
            return 1;
        }
        return 0;
    }
}
