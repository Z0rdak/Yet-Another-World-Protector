package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.core.region.AbstractMarkableRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.*;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;

import java.util.ArrayList;
import java.util.Collection;

public class RegionCommands {

    public static final LiteralArgumentBuilder<CommandSourceStack> REGION_COMMAND = registerRegionCommands();
    public static final LiteralArgumentBuilder<CommandSourceStack> REGIONS_COMMAND = registerRegionsCommands();

    private RegionCommands() {
    }

    public static LiteralArgumentBuilder<CommandSourceStack> registerRegionsCommands(){

        return CommandUtil.literal(CommandConstants.REGIONS);
    }

    // IDEA: Expand command for all area types
    // circle - expand radius
    // ...

    /**
     *
     */
    public static LiteralArgumentBuilder<CommandSourceStack> registerRegionCommands() {
        return CommandUtil.literal(CommandConstants.REGION)
                .executes(ctx -> promptHelp(ctx.getSource()))
                .then(CommandUtil.literal(CommandConstants.HELP)
                        .executes(ctx -> promptHelp(ctx.getSource())));
    }

    private static int removeRegion(CommandSourceStack source, String regionName, ResourceKey<Level> dim) {
        return 0;
    }

    private static int info(CommandSourceStack source, String regionName) {

        return 0;
    }

    private static int setActiveStates(CommandSourceStack source, ResourceKey<Level> dim, boolean activate) {
        try {
            // TODO: Handle errors and give feedback to player
            Player player = source.getPlayerOrException();
            Collection<IMarkableRegion> regionsToProcess = RegionDataManager.get().getRegionsFor(dim);
            RegionDataManager.get().setActiveState(regionsToProcess, activate);
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int setActiveState(CommandSourceStack source, String regionName, ResourceKey<Level> dim, boolean activate) {
        try {
            // TODO: Handle errors and give feedback to player
            Player player = source.getPlayerOrException();
            Collection<IMarkableRegion> regionsToProcess = new ArrayList<>();
            IMarkableRegion region = RegionDataManager.get().getRegionIn(regionName, dim);
            if (region != null) {
                regionsToProcess.add(region);
            }
            RegionDataManager.get().setActiveState(regionsToProcess, activate);
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int setAlertState(CommandSourceStack source, String regionName, boolean mute) {

        return 0;
    }

    private static int promptHelp(CommandSourceStack src) {
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpHeader("help.region.header"));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.1", CommandConstants.REGION, CommandConstants.CREATE));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.2", CommandConstants.REGION, CommandConstants.UPDATE));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.3", CommandConstants.REGION, CommandConstants.REMOVE));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.4", CommandConstants.REGION, CommandConstants.LIST));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.5", CommandConstants.REGION, CommandConstants.INFO));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.6", CommandConstants.REGION, CommandConstants.PRIORITY));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.7", CommandConstants.REGION, CommandConstants.TELEPORT));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.8", CommandConstants.REGION, CommandConstants.DEACTIVATE));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.9", CommandConstants.REGION,  CommandConstants.ALERT));
        return 0;
    }

    private static int promptRegionList(CommandSourceStack source) {
        try {
            promptRegionListForDim(source, source.getPlayerOrException().getCommandSenderWorld().dimension());
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int promptRegionListForDim(CommandSourceStack source, ResourceKey<Level> dim) {

        return 0;
    }

    private static int createRegion(CommandSourceStack source, String regionName, ResourceKey<Level> dim) {
        try {
            createRegion(source, regionName, dim, source.getPlayerOrException());
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int createRegion(CommandSourceStack source, String regionName, ResourceKey<Level> dim, Player owner) {
        try {
            Player player = source.getPlayerOrException();
            ItemStack maybeStick = player.getMainHandItem();
            // TODO: create a method which trhows exception on trying to get stick
            if (StickUtil.isVanillaStick(maybeStick)) {
                StickType stickType = StickUtil.getStickType(maybeStick);
                if (stickType == StickType.MARKER) {
                    CompoundTag stickNBT = StickUtil.getStickNBT(maybeStick);
                    if (stickNBT != null) {
                        AbstractMarkableRegion region = RegionUtil.regionFrom(source.getPlayerOrException(), new MarkerStick(stickNBT), regionName);
                        // TODO
                    }
                }
            }
        } catch (CommandSyntaxException e) {
            CommandUtil.handleCommandWithoutPlayer(e);
        }
        return 0;
    }

    // assumption: regions are only updated with the region marker when in the same dimension
    private static int updateRegion(CommandSourceStack source, String regionName) {
        try {
            Player player = source.getPlayerOrException();
            ItemStack maybeStick = player.getMainHandItem();
            if (StickUtil.isVanillaStick(maybeStick)) {
                try {
                    AbstractStick abstractStick = StickUtil.getStick(maybeStick);
                    if (abstractStick.getStickType() == StickType.MARKER){
                        MarkerStick marker = (MarkerStick) abstractStick;
                        // TODO:
                        //RegionDataManager.get().update(regionName, marker);
                    }
                } catch (StickException e) {
                    e.printStackTrace();
                }
            }
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int removeRegions(CommandSourceStack source, ResourceKey<Level> dim) {
        try {
            //
            Player player = source.getPlayerOrException();
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int listRegionsAround(CommandSourceStack source) {

        return 0;
    }

    private static int teleport(CommandSourceStack source, String regionName, ResourceKey<Level> dim) {
        // use execute in dim for tp because of different dimensions
        return 0;
    }

    private static int setPriority(CommandSourceStack source, String region, int priority) {

        return 0;
    }
}
