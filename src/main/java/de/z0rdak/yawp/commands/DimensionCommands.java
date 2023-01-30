package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.argument.BlockPosArgumentType;
import net.minecraft.command.argument.DimensionArgumentType;
import net.minecraft.command.argument.EntityArgumentType;
import net.minecraft.command.argument.TeamArgumentType;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.registry.RegistryKey;
import net.minecraft.scoreboard.Team;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.text.ClickEvent;
import net.minecraft.text.LiteralTextContent;
import net.minecraft.text.MutableText;
import net.minecraft.text.TranslatableTextContent;
import net.minecraft.util.Formatting;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.util.CommandUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;
import static net.minecraft.util.Formatting.RESET;
import static net.minecraft.util.Formatting.*;

public class DimensionCommands {

    public static final LiteralArgumentBuilder<ServerCommandSource> DIMENSION_COMMAND = register();

    private DimensionCommands() {
    }

    public static LiteralArgumentBuilder<ServerCommandSource> register() {
        List<String> affiliationList = Arrays.asList("member", "owner");
        return literal(DIMENSION)
                /* /wp dimension <dim> list region */
                .then(CommandManager.argument(DIMENSION.toString(), DimensionArgumentType.dimension())
                        .then(literal(CREATE)
                                .then(literal(REGION)
                                        .then(CommandManager.argument(REGION.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(Collections.singletonList("name"), builder))
                                                //.then(CommandManager.argument(AREA.toString(), StringArgumentType.word())
                                                //        .suggests((ctx, builder) -> AreaArgumentType.areaType().listSuggestions(ctx, builder))
                                                //        .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx), getAreaTypeArgument(ctx))))
                                                .then(CommandManager.literal(AreaType.CUBOID.areaType)
                                                        .then(CommandManager.argument("pos1", BlockPosArgumentType.blockPos())
                                                                .then(CommandManager.argument("pos2", BlockPosArgumentType.blockPos())
                                                                        .executes(ctx -> createCuboidRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                BlockPosArgumentType.getBlockPos(ctx, "pos1"),
                                                                                BlockPosArgumentType.getBlockPos(ctx, "pos2"), null))
                                                                        .then(CommandManager.argument(OWNER.toString(), EntityArgumentType.player())
                                                                                .executes(ctx -> createCuboidRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgumentType.getBlockPos(ctx, "pos1"),
                                                                                        BlockPosArgumentType.getBlockPos(ctx, "pos2"), getOwnerArgument(ctx))))))
                                                )
                                        )
                                )
                        )
                        /* /wp dimension <dim> [info] */
                        .executes(ctx -> promptDimensionInfo(ctx.getSource(), getDimCacheArgument(ctx)))
                        .then(literal(INFO).executes(ctx -> promptDimensionInfo(ctx.getSource(), getDimCacheArgument(ctx))))
                        /* /wp dimension <dim> activate */
                        .then(literal(ENABLE)
                                // TODO: Add toggle cmd
                                .executes(ctx -> setActiveState(ctx.getSource(), getDimCacheArgument(ctx), true))
                                .then(CommandManager.argument(ENABLE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setActiveState(ctx.getSource(), getDimCacheArgument(ctx), getEnableArgument(ctx)))))
                        .then(literal(LIST)
                                .then(literal(REGION).executes(ctx -> promptDimensionRegionList(ctx.getSource(), getDimCacheArgument(ctx))))
                                /* /wp dimension <dim> list owner */
                                .then(literal(OWNER).executes(ctx -> promptDimensionPlayerList(ctx.getSource(), getDimCacheArgument(ctx), OWNER)))
                                /* /wp dimension <dim> list member */
                                .then(literal(MEMBER).executes(ctx -> promptDimensionPlayerList(ctx.getSource(), getDimCacheArgument(ctx), MEMBER)))
                                /* /wp dimension <dim> list flag */
                                .then(literal(FLAG).executes(ctx -> promptDimensionFlagList(ctx.getSource(), getDimCacheArgument(ctx)))))
                        .then(literal(DELETE)
                                .then(CommandManager.argument(REGION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> attemptDeleteRegion(ctx.getSource(), getDimCacheArgument(ctx), getRegionArgument(ctx)))
                                        .then(CommandManager.literal("-y")
                                                .executes(ctx -> deleteRegion(ctx.getSource(), getDimCacheArgument(ctx), getRegionArgument(ctx))))))
                        .then(literal(REMOVE)
                                .then(literal(PLAYER)
                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(PLAYER.toString(), EntityArgumentType.player())
                                                        .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(PLAYER.toString(), EntityArgumentType.player())
                                                        .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(TEAM)
                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(TEAM.toString(), TeamArgumentType.team())
                                                        .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgumentType(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(TEAM.toString(), TeamArgumentType.team())
                                                        .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgumentType(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(FLAG)
                                        .then(CommandManager.argument(FLAG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(RegionDataManager.get().getFlagsIdsForDim(getDimCacheArgument(ctx)), builder))
                                                .executes(ctx -> removeFlag(ctx.getSource(), getDimCacheArgument(ctx), StringArgumentType.getString(ctx, FLAG.toString()))))))
                        .then(literal(ADD)
                                .then(literal(PLAYER)
                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(PLAYER.toString(), EntityArgumentType.player())
                                                        .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(PLAYER.toString(), EntityArgumentType.player())
                                                        .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(TEAM)
                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(TEAM.toString(), TeamArgumentType.team())
                                                        .executes(ctx -> addTeam(ctx.getSource(), getTeamArgumentType(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(CommandManager.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(affiliationList, builder))
                                                .then(CommandManager.argument(TEAM.toString(), TeamArgumentType.team())
                                                        .executes(ctx -> addTeam(ctx.getSource(), getTeamArgumentType(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(FLAG)
                                        .then(CommandManager.argument(FLAG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> CommandSource.suggestMatching(RegionFlag.getFlagNames(), builder))
                                                .executes(ctx -> addFlag(ctx.getSource(), getDimCacheArgument(ctx), StringArgumentType.getString(ctx, FLAG.toString())))))));
    }

    public static int checkValidRegionName(String regionName, DimensionRegionCache dimCache) {
        if (!regionName.matches(RegionArgumentType.VALID_NAME_PATTERN.pattern())) {
            return -1;
        }
        if (dimCache.contains(regionName)) {
            return 1;
        }
        return 0;
    }

    private static int createCuboidRegion(ServerCommandSource src, String regionName, DimensionRegionCache dimCache, BlockPos pos1, BlockPos pos2, ServerPlayerEntity owner) {
        int res = checkValidRegionName(regionName, dimCache);
        if (res == -1) {
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.name.invalid", regionName)));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), regionName)));
            return res;
        }
        CuboidRegion region = new CuboidRegion(regionName, new CuboidArea(pos1, pos2), owner, dimCache.dimensionKey());
        if (owner == null) {
            region.setIsActive(false);
        }
        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
        dimCache.addRegion(region);
        LocalRegions.ensureHigherRegionPriorityFor(region, RegionConfig.DEFAULT_REGION_PRIORITY.get());
        RegionDataManager.save();
        sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region))));
        return 0;
    }

    private static int createSphereRegion(ServerCommandSource src, String regionName, DimensionRegionCache dimCache, BlockPos center, BlockPos outerPos, ServerPlayerEntity owner) {
        if (!regionName.matches(RegionArgumentType.VALID_NAME_PATTERN.pattern())) {
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.name.invalid", regionName)));
            return -1;
        }
        if (dimCache.contains(regionName)) {
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.name.exists", dimCache.dimensionKey(), regionName)));
            return 1;
        }
        SphereArea area = new SphereArea(center, outerPos);
        SphereRegion region = new SphereRegion(regionName, area, owner, dimCache.dimensionKey());
        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
        dimCache.addRegion(region);
        RegionDataManager.save();
        sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region))));
        return 0;
    }

    private static int attemptDeleteRegion(ServerCommandSource src, DimensionRegionCache dim, IMarkableRegion region) {
        if (dim.contains(region.getName())) {
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.dim.region.remove.attempt", region.getName(), dim.dimensionKey().getValue())));
            return 0;
        }
        return 1;
    }

    // FIXME: Are child / parent relation properly removed when deleting a region?
    private static int deleteRegion(ServerCommandSource src, DimensionRegionCache dim, IMarkableRegion region) {
        if (dim.contains(region.getName())) {
            if (!region.getChildren().isEmpty()) {
                // TODO: config option which allows deleting region with children? children then default to dim parent
                sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.dim.region.remove.fail.hasChildren", region.getName())));
                return -1;
            }
            if (region.getParent() != null) {
                region.getParent().removeChild(region);
                RegionDataManager.get().cacheFor(region.getDim()).getDimensionalRegion().addChild(region);
            }
            dim.removeRegion(region);
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.dim.region.remove.confirm", region.getName(), dim.dimensionKey().getValue())));
            return 0;
        }
        return 1;
    }

    private static int removeFlag(ServerCommandSource src, DimensionRegionCache dimCache, String flag) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            dimCache.removeFlag(flag);
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.flags.removed", flag, dim.getValue().toString())));
            return 0;
        }
        return 1;
    }

    private static int addFlag(ServerCommandSource src, DimensionRegionCache dimCache, String flag) {
        // FIXME: For now this works because we only have condition flags and no black/whitelist feature
        IFlag iflag = new BooleanFlag(flag, false);
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            dimCache.addFlag(iflag);
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.flags.added", flag, dim.getValue().toString())));
            return 0;
        }
        return 1;
    }

    private static int removePlayer(ServerCommandSource src, ServerPlayerEntity player, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            if (affiliationType.equals(MEMBER.toString())) {
                dimCache.removeMember(player);
            }
            if (affiliationType.equals(OWNER.toString())) {
                dimCache.removeOwner(player);
            }
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.player.removed", affiliationType, player.getEntityName(), dim.getValue().toString())));
        }
        return 0;
    }

    private static int removeTeam(ServerCommandSource src, Team team, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            if (affiliationType.equals(MEMBER.toString())) {
                dimCache.removeMember(team);
            }
            if (affiliationType.equals(OWNER.toString())) {
                dimCache.removeOwner(team);
            }
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.player.removed", affiliationType, team.getName(), dim.getValue().toString())));
            return 0;
        }
        return 1;
    }

    // TODO: If works replace with switch and catch error
    private static int addPlayer(ServerCommandSource src, ServerPlayerEntity player, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            if (affiliationType.equals(MEMBER.toString())) {
                dimCache.addMember(player);
            }
            if (affiliationType.equals(OWNER.toString())) {
                dimCache.addOwner(player);
            }
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.player.added", player.getEntityName(), dim.getValue().toString(), affiliationType)));
            return 0;
        }
        return 1;
    }

    private static int addTeam(ServerCommandSource src, Team team, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.dimensionKey();
            if (affiliationType.equals(MEMBER.toString())) {
                dimCache.addMember(team);
            }
            if (affiliationType.equals(OWNER.toString())) {
                dimCache.addOwner(team);
            }
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.team.added", team.getName(), dim.getValue().toString(), affiliationType)));
            return 0;
        }
        return 1;
    }

    private static int promptDimensionFlagList(ServerCommandSource src, DimensionRegionCache dimCache) {
        List<IFlag> activeFlags = dimCache.getDimensionalRegion().getFlags().stream()
                .filter(IFlag::isActive)
                .sorted()
                .collect(Collectors.toList());
        List<IFlag> inActiveFlags = dimCache.getDimensionalRegion().getFlags().stream()
                .filter(f -> !f.isActive())
                .sorted()
                .toList();
        activeFlags.addAll(inActiveFlags);
        List<IFlag> flags = new ArrayList<>(activeFlags);
        flags.addAll(inActiveFlags);

        RegistryKey<World> dim = dimCache.dimensionKey();
        if (flags.isEmpty()) {
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.flags.empty", dim.getValue().toString())));
            return 1;
        }
        MutableText dimInfoLink = buildDimensionalInfoLink(dim);
        MutableText headerContent = MutableText.of(new TranslatableTextContent("cli.msg.info.region.flag.header", dimInfoLink));
        sendCmdFeedback(src, headerContent);
        flags.forEach(flag -> {
            MutableText removeFlagLink = MutableText.of(new LiteralTextContent(" - "))
                    .append(buildDimensionRemoveFlagLink(flag, dim))
                    .append(MutableText.of(new LiteralTextContent(" '" + flag.getFlagIdentifier() + "' ")));

            sendCmdFeedback(src, removeFlagLink);
        });
        return 0;
    }

    private static int promptDimensionPlayerList(ServerCommandSource src, DimensionRegionCache dimCache, CommandConstants memberOrOwner) {
        if (dimCache != null) {
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            String playerLangKeyPart = memberOrOwner == OWNER ? "owner" : "member";
            String affiliationText = playerLangKeyPart.substring(0, 1).toUpperCase() + playerLangKeyPart.substring(1) + "s";
            MutableText dimInfoLink = buildDimensionalInfoLink(dimRegion.getDim());
            MutableText regionsInDimHeader = MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.list", affiliationText, dimInfoLink));
            sendCmdFeedback(src, regionsInDimHeader);
            sendCmdFeedback(src, buildTeamList(dimRegion, memberOrOwner));
            sendCmdFeedback(src, buildPlayerList(dimRegion, memberOrOwner));
            return 0;
        }
        return 1;
    }

    private static int setActiveState(ServerCommandSource src, DimensionRegionCache dimCache, boolean activate) {
        if (dimCache != null) {
            dimCache.setDimState(activate);

            String langKey = "cli.msg.info.state." + (activate ? "activated" : "deactivated");
            sendCmdFeedback(src, MutableText.of(new TranslatableTextContent(langKey, dimCache.getDimensionalRegion().getDim().getValue().toString())));
            return 0;
        }
        return 1;
    }

    private static int promptDimensionRegionList(ServerCommandSource source, DimensionRegionCache dimCache) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.getDimensionalRegion().getDim();
            List<IMarkableRegion> regionsForDim = dimCache.regionsInDimension
                    .values()
                    .stream()
                    .sorted(Comparator.comparing(IMarkableRegion::getName))
                    .collect(Collectors.toList());
            if (regionsForDim.isEmpty()) {
                sendCmdFeedback(source, MutableText.of(new TranslatableTextContent("cli.msg.dim.info.regions.empty", dim.getValue().toString())));
                return -1;
            }
            MutableText dimInfoLink = buildDimensionalInfoLink(dim);
            MutableText regionsInDimHeader = MutableText.of(new TranslatableTextContent("cli.msg.dim.info.region.list.header", dimInfoLink));
            sendCmdFeedback(source, regionsInDimHeader);
            // TODO: Pagination for more than x regions
            regionsForDim.forEach(region -> {
                MutableText regionRemoveLink = MutableText.of(new LiteralTextContent(" - "))
                        .append(buildDimSuggestRegionRemovalLink(region))
                        .append(" ")
                        .append(buildRegionInfoLink(region))
                        .append(dimCache.getDimensionalRegion().hasChild(region)
                                ? MessageUtil.buildTextWithHoverMsg(MutableText.of(new LiteralTextContent("*")), MutableText.of(new TranslatableTextContent("cli.msg.info.dim.region.child.hover")), GOLD)
                                : MutableText.of(new LiteralTextContent("")))
                        .append(MutableText.of(new LiteralTextContent(RESET + " @ " + RESET)))
                        .append(buildRegionTeleportLink(region));
                sendCmdFeedback(source, regionRemoveLink);
            });
            return 0;
        }
        return 1;
    }

    /* Used for dimension info */
    private static void promptDimensionOwners(ServerCommandSource src, DimensionalRegion dimRegion) {
        // [n player(s)] [+]
        PlayerContainer owners = dimRegion.getOwners();
        MutableText playersAddLink = buildDimAddPlayerLink(dimRegion, "cli.msg.dim.info.players.add",
                OWNER);
        MutableText players = owners.hasPlayers()
                ? buildDimPlayerListLink(dimRegion, owners, OWNER)
                : MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.list.link.text", owners.getPlayers().size()));
        players.append(playersAddLink);

        // [n team(s)] [+]
        MutableText teamAddLink = buildDimAddTeamLink(dimRegion, "cli.msg.dim.info.teams.add",
                OWNER);
        MutableText teams = owners.hasTeams()
                ? buildDimTeamListLink(dimRegion, owners, OWNER)
                : MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.list.link.text", owners.getTeams().size()));
        teams.append(teamAddLink);

        // Owners: [n player(s)] [+], [n team(s)] [+]
        MutableText dimOwners = MutableText.of(new TranslatableTextContent("cli.msg.dim.info.owners"))
                .append(MutableText.of(new LiteralTextContent(": ")))
                .append(players).append(MutableText.of(new LiteralTextContent(", ")))
                .append(teams);
        sendCmdFeedback(src, dimOwners);
    }

    private static int promptDimensionRegions(ServerCommandSource source, DimensionRegionCache dimCache) {
        if (dimCache != null) {
            RegistryKey<World> dim = dimCache.getDimensionalRegion().getDim();
            List<IMarkableRegion> regionsForDim = dimCache.regionsInDimension
                    .values()
                    .stream()
                    .sorted(Comparator.comparing(IMarkableRegion::getName))
                    .toList();
            MutableText regions = MutableText.of(new TranslatableTextContent("cli.msg.info.dim.region")).append(": ");
            if (regionsForDim.isEmpty()) {
                regions.append(MutableText.of(new TranslatableTextContent("cli.msg.dim.info.regions.empty", dim.getValue().toString())));
            } else {
                regions.append(buildDimRegionListLink(dimCache, dimCache.getDimensionalRegion()));
            }
            sendCmdFeedback(source, regions);
            return 0;
        }
        return 1;
    }

    private static void promptDimensionMembers(ServerCommandSource src, DimensionalRegion dimRegion) {
        // [n player(s)] [+]
        PlayerContainer members = dimRegion.getMembers();
        MutableText playersAddLink = buildDimAddPlayerLink(dimRegion, "cli.msg.dim.info.players.add",
                MEMBER);
        MutableText players = members.hasPlayers() ?
                buildDimPlayerListLink(dimRegion, members, MEMBER)
                : MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.player.list.link.text", members.getPlayers().size()));
        players.append(playersAddLink);

        // [n team(s)] [+]
        MutableText teamAddLink = buildDimAddTeamLink(dimRegion, "cli.msg.dim.info.teams.add",
                MEMBER);
        MutableText teams = members.hasTeams()
                ? buildDimTeamListLink(dimRegion, members, MEMBER)
                : MutableText.of(new TranslatableTextContent("cli.msg.info.region.affiliation.team.list.link.text", members.getTeams().size()));
        teams.append(teamAddLink);

        // Members: [n player(s)] [+], [n team(s)] [+]
        MutableText dimMembers = MutableText.of(new TranslatableTextContent("cli.msg.dim.info.members"))
                .append(MutableText.of(new LiteralTextContent(": ")))
                .append(players).append(MutableText.of(new LiteralTextContent(", ")))
                .append(teams);
        sendCmdFeedback(src, dimMembers);
    }

    private static void promptDimensionFlags(ServerCommandSource src, DimensionalRegion dimRegion) {
        MutableText dimFlagMessage = MutableText.of(new TranslatableTextContent("cli.msg.dim.info.flags"));
        MutableText flags = dimRegion.getFlags().isEmpty()
                ? MutableText.of(new TranslatableTextContent("cli.msg.info.region.flag.link.text", dimRegion.getFlags().size()))
                : buildDimFlagListLink(dimRegion);
        dimFlagMessage.append(MutableText.of(new LiteralTextContent(": ")))
                .append(flags)
                .append(buildAddDimFlagLink(dimRegion));
        sendCmdFeedback(src, dimFlagMessage);
    }

    private static void promptDimensionState(ServerCommandSource src, AbstractRegion region, String command) {
        String onClickAction = region.isActive() ? "deactivate" : "activate";
        String hoverText = "cli.msg.info.state." + onClickAction;
        String linkText = "cli.msg.info.state.link." + (region.isActive() ? "activate" : "deactivate");
        Formatting color = region.isActive() ? GREEN : RED;
        MutableText stateLink = buildExecuteCmdComponent(linkText, hoverText, command, ClickEvent.Action.RUN_COMMAND, color);
        sendCmdFeedback(src, MutableText.of(new TranslatableTextContent("cli.msg.info.state"))
                .append(MutableText.of(new LiteralTextContent(": ")))
                .append(stateLink));
    }

    private static int promptDimensionInfo(ServerCommandSource src, DimensionRegionCache dimCache) {
        // Dimension info header
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        MutableText clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.dim.overview.header.dump.link.text", "cli.msg.dim.overview.header.dump.link.hover", NbtHelper.toPrettyPrintedText(dimRegion.serializeNBT()).getString(), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
        MutableText dimInfoHeader = MutableText.of(new TranslatableTextContent("cli.msg.dim.overview.header", clipBoardDumpLink, buildDimensionalInfoLink(dimRegion.getDim())));
        sendCmdFeedback(src, dimInfoHeader);

        // Regions in dimension
        // TODO: Change [n region(s)] to [n region(s)] [+]s
        promptDimensionRegions(src, dimCache);

        // Dimension owners & members
        promptDimensionOwners(src, dimRegion);
        promptDimensionMembers(src, dimRegion);

        // Flags: [n flag(s)] [+]
        promptDimensionFlags(src, dimRegion);

        // State: [activated]
        String command = "/" + CommandPermissionConfig.BASE_CMD + " " + DIMENSION + " " + dimRegion.getName() + " " + ENABLE + " " + !dimRegion.isActive();
        promptDimensionState(src, dimRegion, command);
        return 0;
    }
}
